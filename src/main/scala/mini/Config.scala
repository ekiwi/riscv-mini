// See LICENSE for license details.

package mini

import chisel3.Module
import config.{Config, Parameters}
import junctions._

class MiniConfig
    extends Config((site, here, up) => {
      // Core
      case XLEN        => 32
      case Trace       => false
      case BuildALU    => (_: Parameters) => Module(new AluArea(32))
      case BuildImmGen => (_: Parameters) => Module(new ImmGenWire(32))
      case BuildBrCond => (_: Parameters) => Module(new BrCondArea(32))
      // Cache
      case NWays           => 1 // TODO: set-associative
      case NSets           => 256
      case CacheBlockBytes => 4 * (here(XLEN) >> 3) // 4 x 32 bits = 16B
      // NastiIO
      case NastiKey => new NastiParameters(idBits = 5, dataBits = 64, addrBits = here(XLEN))
    })
