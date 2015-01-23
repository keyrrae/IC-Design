`timescale 1ns/1ns
module divide_by_5 (clk_in, reset, clk_out);

	input clk_in;
	input  reset;
	output clk_out;

	reg [2:0] counter;
	reg tff_1, tff_2;
	wire tff_1en, tff_2en;
	wire clk_b;
	wire clk_out;

	assign clk_b = ~ clk_in;
	assign tff_1en = (counter == 3'b000) ? 1'b1 : 1'b0;
	assign tff_2en = (counter == 3'b011) ? 1'b1 : 1'b0;

	always @ (posedge clk_in)
    	if (reset) begin
        	counter <= 3'b000;
			tff_1 <= 1'b0;
    	end
    	else begin 
        	counter <= (counter == 3'b100) ? 3'b000 : counter + 3'b001;
        	tff_1 <= (tff_1en) ? (~ tff_1) : tff_1;
    	end
        

	always @ (posedge clk_b)
    	if (reset)
        	tff_2 <= 1'b0;
    	else 
        	tff_2 <= (tff_2en) ? (~ tff_2) : tff_2;

	assign clk_out = tff_1 ^ tff_2;

endmodule
