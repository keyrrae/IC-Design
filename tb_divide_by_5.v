`timescale 1ns/1ns
module tb_divide_by_5;

	reg clk, rst;
	wire clk_div_by_5;

	divide_by_5 dut(clk, rst, clk_div_by_5);

	initial begin
		rst = 1'b1;
		clk = 1'b0;
		#30 rst = 1'b0;
		#1600 $stop;
	end

	always #20 clk = ~ clk;

endmodule
