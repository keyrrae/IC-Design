`timescale 1ns/1ps

`include "constants.vams"
`include "disciplines.vams"


module amp_core(

	 AVDD_3P3V,
	 AVSS,
	 PBKG,
	
	 AVDD_PWR_3P3V,
	 AVSS_PWR,
	 PBKG_PWR,

	 opa_inp_a_3P3V,
	 opa_inn_a_3P3V,
	
	 rdcpwr_i_3P3V,
	 en_i_3P3V,
     ibopa_up_0p5uA_a_3P3V,
	
	 cmpnstn_out_a_3P3V,
//	 opa_out_ext_a_3P3V,
//	 opa_out_int_a_3P3V
	 opa_out_a_3P3V
	);
	
    input AVDD_3P3V;
	input AVSS;
	input PBKG;
	
	input AVDD_PWR_3P3V;
	input AVSS_PWR;
	input PBKG_PWR;

	input opa_inp_a_3P3V;
	input opa_inn_a_3P3V;
	
	input rdcpwr_i_3P3V;
	input en_i_3P3V;
  input ibopa_up_0p5uA_a_3P3V;
	
	inout cmpnstn_out_a_3P3V;
	inout opa_out_a_3P3V;

	//============ wreal definitions =================
		//Supplies
			wreal AVDD_3P3V;
			wreal AVSS;
			wreal PBKG;
	
			wreal AVDD_PWR_3P3V;
			wreal AVSS_PWR;
			wreal PBKG_PWR;
		
		// Analog Signals
			wreal opa_inp_a_3P3V;
			wreal opa_inn_a_3P3V;
			wreal cmpnstn_out_a_3P3V;
			wreal opa_out_a_3P3V;

		// Bias & Vref Signals (Ana)
  			wreal ibopa_up_0p5uA_a_3P3V;
		
   	//========== Internal signal definitions ===========
		real opa_inp_s, opa_inn_s;
		real opa_out_next, opa_out_present;
        real trans_step;

        reg clk_int;
        reg out_state_ind;

	//=============== Event Definition =================
  //		reg COMPNSTN_FAULT, POWER_NOT_READY, IBIAS_NOT_READY;
	//================ Parameters ====================== 
	 	// Supply defaults                                                 
			parameter real p_V_AVDD = `ifdef def_V_AVDD `def_V_AVDD; `else 3.0; `endif
			parameter real p_V_AVSS = `ifdef def_V_AVSS `def_V_AVSS; `else 0.0; `endif

		// Supply Tresholds
			parameter real p_Vt_3P3V = `ifdef def_Vt_3P3V `def_Vt_3P3V; `else 1.75; `endif
			parameter real p_Vt_VSS  = `ifdef def_Vt_VSS  `def_Vt_VSS;  `else 0.1; `endif

			parameter real p_It_hi_opa_IBIAS  = `ifdef def_It_hi_opa_IBIAS  `def_It_hi_opa_IBIAS;  `else 600e-9; `endif
			parameter real p_It_lo_opa_IBIAS  = `ifdef def_It_lo_opa_IBIAS  `def_It_lo_opa_IBIAS;  `else 400e-9; `endif

		// Amplifier paremeters
			//parameter real diff_tol = 40.0e-3; // input difference tolarence 100mV
			
			parameter real lrgsgnl_Vt_p = 200.0e-3;
			parameter real lrgsgnl_Vt_n = -200.0e-3;

			parameter real smllsgnl_Vt_p = 5.0e-3;
			parameter real smllsgnl_Vt_n = -5.0e-3;

		//case statement 
            parameter FINAL = 5'b0_0001;
			parameter SMLLSGNLN = 5'b0_0010;
			parameter SMLLSGNLP = 5'b0_0100; 
			parameter LRGSGNLN = 5'b0_1000; 
			parameter LRGSGNLP = 5'b1_0000; 

			parameter DISABLE = 3'b000;
			parameter NORMAL = 3'b001; 
			parameter OUTCLIP_P = 3'b010;
			parameter OUTCLIP_N = 3'b011;
			parameter INPUTCLIP = 3'b100;


	//	wreal trans_step_p;
    //  wreal trans_step_n;
		wreal OUTPUT_CLIP_Vt_p;
		wreal OUTPUT_CLIP_Vt_n;

	//================ Power Flags =====================
		wire AVDD_ok_s, AVSS_ok_s, PBKG_ok_s, power_ok_s, IBIAS_ok_s;
		assign AVDD_ok_s = (AVDD_3P3V > p_Vt_3P3V & AVDD_PWR_3P3V > p_Vt_3P3V ) ? 1'b1 : 1'b0;
		assign AVSS_ok_s = (AVSS < p_Vt_VSS & AVSS_PWR < p_Vt_VSS) ? 1'b1 : 1'b0;
		assign PBKG_ok_s = (PBKG < p_Vt_VSS & PBKG_PWR < p_Vt_VSS) ? 1'b1 : 1'b0;
		assign power_ok_s = (AVDD_ok_s & AVSS_ok_s & PBKG_ok_s);
		assign OUTPUT_CLIP_Vt_p = AVDD_PWR_3P3V - 100.0e-3;
		assign OUTPUT_CLIP_Vt_n = AVSS_PWR + 100.0e-3;

	//================ Net Connection Flags=============
        wire cmpnstn_ok_s;  

        assign cmpnstn_ok_s = ( cmpnstn_out_a_3P3V - opa_out_a_3P3V < 10.0e-3 ||
             					cmpnstn_out_a_3P3V - opa_out_a_3P3V > -10.0e-3 || 
                                cmpnstn_out_a_3P3V === opa_out_a_3P3V );

	//================ Bias Flags ====================== 
		assign IBIAS_ok_s = (ibopa_up_0p5uA_a_3P3V < p_It_hi_opa_IBIAS 
							&& ibopa_up_0p5uA_a_3P3V > p_It_lo_opa_IBIAS);
    //================== Range Flags ===================
		real opa_inp_int, opa_inn_int, vin_diff_int;
	    wire lrgsgnl_p, lrgsgnl_n, smllsgnl_p, smllsgnl_n, fnlsgnl;
		wire [4:0] range_flag;

        always @(*) begin
            opa_inp_int = (opa_inp_a_3P3V === `wrealZState || opa_inp_a_3P3V === `wrealXState) ?
                            0.5*AVDD_3P3V : opa_inp_a_3P3V;
            opa_inn_int = (opa_inn_a_3P3V === `wrealZState || opa_inn_a_3P3V === `wrealXState) ? 
                            0.5*AVDD_3P3V : opa_inn_a_3P3V;
            vin_diff_int = opa_inp_int - opa_inn_int;
        end

       	assign lrgsgnl_p = (vin_diff_int > lrgsgnl_Vt_p);
		assign lrgsgnl_n = (vin_diff_int < lrgsgnl_Vt_n);

		assign smllsgnl_p = (vin_diff_int > smllsgnl_Vt_p && 
                            (vin_diff_int < lrgsgnl_Vt_p || vin_diff_int == lrgsgnl_Vt_p));

        assign smllsgnl_n = (vin_diff_int < smllsgnl_Vt_n && 
                            (vin_diff_int > lrgsgnl_Vt_n ||vin_diff_int == lrgsgnl_Vt_n));
                            
		assign fnlsgnl = (vin_diff_int >= smllsgnl_Vt_n && 
                         (vin_diff_int < smllsgnl_Vt_p || vin_diff_int == smllsgnl_Vt_p));
		
		assign range_flag = {lrgsgnl_p, lrgsgnl_n, smllsgnl_p, smllsgnl_n, fnlsgnl};
	
    //================ Clippling Flags ================= 	
		wire INPUT_CLIP_s,OUTPUT_CLIP_p, OUTPUT_CLIP_n;  
		assign INPUT_CLIP_s = (opa_inp_a_3P3V > AVDD_3P3V) ||
	   						  (opa_inp_a_3P3V < AVSS) ||
							  (opa_inn_a_3P3V > AVDD_3P3V) ||
							  (opa_inn_a_3P3V < AVSS) ;
        assign OUTPUT_CLIP_p = (opa_out_a_3P3V >= OUTPUT_CLIP_Vt_p);
        assign OUTPUT_CLIP_n = (opa_out_a_3P3V < OUTPUT_CLIP_Vt_n) ||
                               (opa_out_a_3P3V == OUTPUT_CLIP_Vt_n) ;

	initial begin
//#10000;
      // assigning a VDD/2 to output at initial state
      //  trans_step_p = 30.0e-3;
        //trans_step_n = 30.0e-3;
        //opa_out_next = 0.5 * AVDD_PWR_3P3V;
		clk_int = 1'b0;
       // trans_step = 0.1;d
	end

	always #5 clk_int = ~clk_int;
		
	always @(*)
	    //trans_step <= trans_step + 0.1;
      	case (range_flag)
			FINAL : trans_step = 0.0;  
            SMLLSGNLN : trans_step = (rdcpwr_i_3P3V) ? -3.0e-3 : -7.0e-3;  
                        // 3mV(reduced power) or 7mV time step every 10ns, small signal

			SMLLSGNLP : trans_step = (rdcpwr_i_3P3V) ? 3.0e-3 : 7.0e-3;    
                        // 3mV(reduced power) or 7mV time step every 10ns, small signal
            LRGSGNLN : trans_step = (rdcpwr_i_3P3V) ? -11.0e-3 : -31.0e-3; 
                        // 31mV time step every 10ns, 3.1V/us slew rate

			LRGSGNLP : trans_step = (rdcpwr_i_3P3V) ? 11.0e-3 : 31.0e-3;   
                        // 31mV time step every 10ns, 3.1V/us slew rate

			default : trans_step = 0.0;
		endcase

	reg [2:0] pstate, nstate;
	

	always @(posedge clk_int)
		if (!en_i_3P3V || !power_ok_s || !IBIAS_ok_s || !cmpnstn_ok_s) begin
			pstate <= DISABLE;
			opa_out_present <= 0.5*AVDD_PWR_3P3V;
            opa_out_next <= 0.5*AVDD_PWR_3P3V;
		end
		else begin
			pstate <= nstate;
			opa_out_present <= opa_out_next;
		end

//	always @(posedge clk_int)
//		case (pstate)
//			DISABLE  : $display("Not enabled or not OK, at %t",$time);
//			NORMAL  : ; 
//			OUTCLIP_P : $display("Positively output clipping, at %t",$time);
//			OUTCLIP_N : $display("Negatively output clipping, at %t",$time);
//			INPUTCLIP : $display("Input clipping, at %t",$time);
//           default:;
//        endcase

	always @(*)
		case(pstate)
			DISABLE:
					if (en_i_3P3V && power_ok_s && IBIAS_ok_s && cmpnstn_ok_s) begin
							nstate = NORMAL;
							opa_out_next = 0.5*AVDD_PWR_3P3V;
                            out_state_ind = 1'b1;
                    end
					else begin
							nstate = DISABLE;
							opa_out_next = 0.5*AVDD_PWR_3P3V;
                            out_state_ind = 1'bz;
					end
			NORMAL : 
					if (INPUT_CLIP_s) begin
						 nstate = INPUTCLIP;
						 opa_out_next = 0.5 * AVDD_PWR_3P3V;
                         out_state_ind = 1'bx;
					end
					else if (opa_out_present + trans_step >= AVDD_PWR_3P3V - 1e-3) begin
							nstate = OUTCLIP_P;
							opa_out_next = AVDD_PWR_3P3V - 1e-3;
                            out_state_ind = 1'b1;
					end
					else if ((opa_out_present + trans_step) <= AVSS_PWR + 1e-3) begin
							nstate = OUTCLIP_N;
							opa_out_next = AVSS_PWR + 1e-3;
                            out_state_ind = 1'b1;
					end
					else begin
							nstate = NORMAL;
							opa_out_next = opa_out_present + trans_step;
                            out_state_ind = 1'b1;
					end

			OUTCLIP_P:
					if (INPUT_CLIP_s) begin
						 nstate = INPUTCLIP;
						 opa_out_next = 0.5 * AVDD_PWR_3P3V;
                         out_state_ind = 1'bx;
    				end
					else if (opa_out_present + trans_step >= AVDD_PWR_3P3V - 1e-3) begin
							nstate = OUTCLIP_P;
							opa_out_next = AVDD_PWR_3P3V - 1e-3;
              out_state_ind = 1'b1;
					end
					else if ((opa_out_present + trans_step) <= AVSS_PWR + 1e-3) begin
							nstate = OUTCLIP_N;
							opa_out_next = AVSS_PWR + 1e-3;
              out_state_ind = 1'b1;
					end
					else begin
							nstate = NORMAL;
							opa_out_next = opa_out_present + trans_step;
              out_state_ind = 1'b1;
					end
			OUTCLIP_N:
					if (INPUT_CLIP_s) begin
						 nstate = INPUTCLIP;
						 opa_out_next = 0.5 * AVDD_PWR_3P3V;
                         out_state_ind = 1'bx;
					end
					else if (opa_out_present + trans_step >= AVDD_PWR_3P3V - 1e-3) begin
							nstate = OUTCLIP_P;
							opa_out_next = AVDD_PWR_3P3V - 1e-3;
              out_state_ind = 1'b1;
					end
					else if ((opa_out_present + trans_step) <= AVSS_PWR + 1e-3) begin
							nstate = OUTCLIP_N;
							opa_out_next = AVSS_PWR + 1e-3;
                            out_state_ind = 1'b1;
					end
					else begin
							nstate = NORMAL;
                            opa_out_next = opa_out_present + trans_step;
                            out_state_ind = 1'b1;
					end

			INPUTCLIP:
					if (INPUT_CLIP_s) begin
						 nstate = INPUTCLIP;
						 opa_out_next = 0.5*AVDD_PWR_3P3V;
                         out_state_ind = 1'bx;
					end
					else if (opa_out_present + trans_step >= AVDD_PWR_3P3V - 1e-3) begin
							nstate = OUTCLIP_P;
							opa_out_next = AVDD_PWR_3P3V - 1e-3;
                            out_state_ind = 1'b1;
					end
					else if ((opa_out_present + trans_step) <= AVSS_PWR + 1e-3) begin
							nstate = OUTCLIP_N;
							opa_out_next = AVSS_PWR + 1e-3;
                            out_state_ind = 1'b1;
					end
					else begin
							nstate = NORMAL;
							opa_out_next = opa_out_present + trans_step;
                            out_state_ind = 1'b1;

					end
			default : begin
							nstate = DISABLE;
							opa_out_next = 0.5 * AVDD_PWR_3P3V;
                            out_state_ind = 1'bz;
					end
		endcase

	assign opa_out_a_3P3V = (out_state_ind === 1'bz || out_state_ind === 1'b0) ?
                             `wrealZState : ((out_state_ind === 1'bx) ?
                             `wrealXState : opa_out_present);

endmodule
