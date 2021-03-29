// See README.md for license details.

package mini

import java.io._

import chisel3._
import chisel3.experimental.RunFirrtlTransform
import chisel3.stage.phases.AspectPhase
import chisel3.stage.{ChiselCircuitAnnotation, ChiselStage, DesignAnnotation, NoRunFirrtlCompilerAnnotation}
import chisel3.testers.BasicTester
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.Unserializable
import firrtl.transforms.BlackBoxSourceHelper.writeResourceToDirectory
import firrtl.{Driver => _, _}
import treadle.chronometry.Timer
import treadle.stage.TreadleTesterPhase
import treadle.{TreadleTesterAnnotation, WriteVcdAnnotation}
import chisel3.experimental.BaseModule
import chisel3.internal.firrtl.Circuit

object TesterDriver extends BackendCompilationUtilities {

  trait Backend extends NoTargetAnnotation with Unserializable
  case object VerilatorBackend extends Backend
  case object TreadleBackend extends Backend

//  val defaultBackend: Backend = TreadleBackend
  val defaultBackend: Backend = VerilatorBackend

  /** For use with modules that should successfully be elaborated by the
    * frontend, and which can be turned into executables with assertions. */
  def execute(t:                    () => BasicTester,
              additionalVResources: Seq[String] = Seq(),
              annotations:          AnnotationSeq = Seq(),
              nameHint:             Option[String] = None): Boolean = {


    val adjustedAnnotations: AnnotationSeq = if (annotations.exists(_.isInstanceOf[Backend])) {
      annotations
    } else {
      annotations :+ defaultBackend
    }
    adjustedAnnotations.map {
      case TreadleBackend =>
        executeTreadle(t, additionalVResources, annotations, nameHint)
      case VerilatorBackend =>
        executeVerilog(t, additionalVResources, annotations, nameHint)
    }.reduce(_ && _)
  }

  /** For use with modules that should successfully be elaborated by the
    * frontend, and which can be turned into executables with assertions.
    */
  def executeVerilog(t:                    () => BasicTester,
                     additionalVResources: Seq[String] = Seq(),
                     annotations:          AnnotationSeq = Seq(),
                     nameHint:             Option[String] = None): Boolean = {


    val generatorAnnotation = chisel3.stage.ChiselGeneratorAnnotation(finishWrapper(t))
    val elaboratedAnno = (new chisel3.stage.phases.Elaborate).transform(annotations :+ generatorAnnotation)
    val circuit = elaboratedAnno.collect { case x: ChiselCircuitAnnotation => x }.head.circuit
    val dut = elaboratedAnno.collectFirst { case d: DesignAnnotation[_] => d }.get

    // Set up a bunch of file handlers based on a random temp filename,
    // plus the quirks of Verilator's naming conventions
    val target = circuit.name

    val targetName = s"test_run_dir/${circuit.name}" + (nameHint match {
      case Some(hint) => s"_$hint"
      case _ => ""
    })

    FileUtils.makeDirectory(targetName)
    val path = new File(targetName)

//    val path = createTestDirectory(target)
    val fname = new File(path, target)

    // For now, dump the IR out to a file
    val compiledAnnotations = (new ChiselStage).execute(
      Array("-E", "chirrtl"),
      elaboratedAnno :+ firrtl.options.TargetDirAnnotation(targetName)
    )

    val firrtlCircuit = Driver.toFirrtl(circuit)

    // Copy CPP harness and other Verilog sources from resources into files
    val cppHarness =  new File(path, "top.cpp")
    copyResourceToFile("/chisel3/top.cpp", cppHarness)
    // NOTE: firrtl.Driver.execute() may end up copying these same resources in its BlackBoxSourceHelper code.
    // As long as the same names are used for the output files, and we avoid including duplicate files
    //  in BackendCompilationUtilities.verilogToCpp(), we should be okay.
    // To that end, we use the same method to write the resource to the target directory.
    val additionalVFiles = additionalVResources.map((name: String) => {
      writeResourceToDirectory(name, path)
    })

    // Compile firrtl
    val transforms = circuit.annotations.collect {
      case anno: RunFirrtlTransform => anno.transformClass
    }.distinct
      .filterNot(_ == classOf[Transform])
      .map { transformClass: Class[_ <: Transform] => transformClass.newInstance() }
    val newAnnotations = circuit.annotations.map(_.toFirrtl).toList ++ annotations ++ Seq(dut)
    val resolvedAnnotations = new AspectPhase().transform(newAnnotations).toList
    val optionsManager = new ExecutionOptionsManager("chisel3") with HasChiselExecutionOptions with HasFirrtlOptions {
      commonOptions = CommonOptions(topName = target, targetDirName = path.getAbsolutePath)
      firrtlOptions = FirrtlExecutionOptions(compilerName = "verilog", annotations = resolvedAnnotations,
        customTransforms = transforms,
        firrtlCircuit = Some(firrtlCircuit))
    }
    firrtl.Driver.execute(optionsManager) match {
      case _: FirrtlExecutionFailure => return false
      case _ =>
    }

    // Use sys.Process to invoke a bunch of backend stuff, then run the resulting exe
    if ((verilogToCpp(target, path, additionalVFiles, cppHarness) #&&
      cppToExe(target, path)).! == 0) {

      val timer = new Timer

      val result = timer("test") {
        executeExpectingSuccess(target, path)
      }
      println(timer.report())
      result
    } else {
      false
    }
  }

  def executeTreadle(t:                    () => BasicTester,
                     additionalVResources: Seq[String] = Seq(),
                     annotations:          AnnotationSeq = Seq(),
                     nameHint:             Option[String] = None): Boolean = {
    val generatorAnnotation = chisel3.stage.ChiselGeneratorAnnotation(t)

    // This provides an opportunity to translate from top level generic flags to backend specific annos
    var annotationSeq = annotations :+ WriteVcdAnnotation

    // This produces a chisel circuit annotation, a later pass will generate a firrtl circuit
    // Can't do both at once currently because generating the latter deletes the former
    annotationSeq = (new chisel3.stage.phases.Elaborate).transform(annotationSeq :+ generatorAnnotation)

    val circuit = annotationSeq.collect { case x: ChiselCircuitAnnotation => x }.head.circuit

    val targetName = s"test_run_dir/${circuit.name}" + (nameHint match {
      case Some(hint) => s"_$hint"
      case _          => ""
    }) + "_treadle"

    annotationSeq = annotationSeq :+ TargetDirAnnotation(targetName)

    // This generates the firrtl circuit needed by the TreadleTesterPhase
    annotationSeq = (new ChiselStage).run(
      annotationSeq ++ Seq(NoRunFirrtlCompilerAnnotation)
    )

    // This generates a TreadleTesterAnnotation with a treadle tester instance
    annotationSeq = TreadleTesterPhase.transform(annotationSeq)

    val treadleTester = annotationSeq.collectFirst { case TreadleTesterAnnotation(t) => t }.getOrElse(
      throw new Exception(
        s"TreadleTesterPhase could not build a treadle tester from these annotations" +
          annotationSeq.mkString("Annotations:\n", "\n  ", "")
      )
    )

    try {
      var cycle = 0L
      while (true) {
        cycle += 1
        if (cycle % 10000 == 0) {
          println(s"Cycle $cycle")
        }
        treadleTester.step()
      }
    } catch {
      case t: Throwable =>
    }
    treadleTester.finish
    treadleTester.report()

    treadleTester.getStopResult match {
      case None    => true
      case Some(0) => true
      case _       => false
    }
  }

  /**
    * Calls the finish method of an BasicTester or a class that extends it.
    * The finish method is a hook for code that augments the circuit built in the constructor.
    */
  def finishWrapper(test: () => BasicTester): () => BasicTester = { () =>
    {
      val tester = test()
      tester.finish()
      tester
    }
  }
}
