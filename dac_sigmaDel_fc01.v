module dac_sigmaDel_fc01(clk,
                         rst_sd,
                         disable_innoise,
                         din,
                         noise,
                         sd_out);


// A 2nd-order 1b sigma-delta DAC.  Input is 16 bits.

// name             type    size    description
//------------------------------------------------------------------------------
// clk             input   1       system clock
// rst_sd          input   1       asynchronous reset
// disable_innoise input   1       asynchronous disable of noise to input summer
// din             input   [15:0]  unsigned input word
// noise           input   [ 3:0]  signed random noise
// sd_out          output  1       control for analog current switche
//
// Module is written for IEEE Verilog 2001


input                     clk;
input                  rst_sd;
input         disable_innoise;
input  [15:0]             din;
input  [ 3:0]           noise;
output                 sd_out;


//regs and wires below correspond to signal names in dac_fc00.mdl

reg      signed [18:0]     z1;  //1st integrator
reg      signed [17:0]     z2;  //2nd integrator
reg                        z3;  //quantizer output


wire                  innoise;  //noise to input summer
wire     signed [16:0]     s1;  //subtractor
wire     signed [19:0]     s2;  //1st integrator
wire     signed [18:0]  s2Sat;  //1st integrator saturated
wire     signed [17:0]     s3;  //subtractor
wire     signed [18:0]     s4;  //2nd integrator
wire     signed [17:0]  s4Sat;  //2nd integrator saturated
wire     signed [ 8:0]     s5;  //add random noise


//register update and reset statements:
always @(posedge clk or posedge rst_sd)
  if(rst_sd)
    begin
      z1    <= #1 18'h0_0000;
      z2    <= #1 18'h0_0000;
      z3    <= #1          0;
    end
  else
    begin
      z1    <= #1  s2Sat;
      z2    <= #1  s4Sat;
      z3    <= #1 ~s5[8];
    end


//Assignments:
assign  innoise = !disable_innoise && noise[0]                                      ;
assign       s1 = s5 >= 0 ? din - 65536 + innoise : din + innoise                   ;
assign       s2 = s1 + z1                                                           ;
assign    s2Sat = ~|s2[19:18] || &s2[19:18] ? s2 : s2[19] ? 19'h4_0000 : 19'h3_ffff ;
assign       s3 = s5 >= 0 ?  $signed(z1[18:1]) - 65536 : $signed(z1[18:1])          ;
assign       s4 = s3 + z2                                                           ;
assign    s4Sat = ~|s4[18:17] || &s4[18:17] ? s4 : s4[18] ? 18'h2_0000 : 18'h1_ffff ;
assign       s5 = $signed(z2[17:10]) + $signed(noise)                               ;
assign   sd_out = z3                                                                ;

endmodule  //dac_sigmaDel_fc01
