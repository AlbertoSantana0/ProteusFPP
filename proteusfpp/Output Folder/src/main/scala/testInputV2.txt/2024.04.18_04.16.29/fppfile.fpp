module Fw { 
 port Cmd 
 port CmdReg 
 port CmdResponse 
 port Log 
 port LogText 
 port Tlm 
 port PrmGet 
 port PrmSet 
 port Time 
 } 
 
 
 @ An array of 3 F64 values 
 array F64x3 = [ 3 ] F64 
 
 @ Component for illustrating telemetry channel update specifiers 
 passive component TlmUpdate { 
 
 
 
 
 
 @ Telemetry port 
 telemetry port tlmOut 
 
 @ Time get port 
 time get port timeGetOut 
 
 
 
 
 
 @ Telemetry channel 1 
 @ Always emitted 
 telemetry Channel1 : U32 
 
 @ Telemetry channel 2 
 @ Emitted on change 
 telemetry Channel2 : F64 id 0x10 update on change 
 
 @ Telemetry channel 3 
 @ Always emitted 
 telemetry Channel3 : F64x3 update always 
 
 } 
 
 
 
 
