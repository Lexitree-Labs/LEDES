open Angstrom
open Common

type input = string
type err = string

let header_validator = validate_ledes98bi_header_greedy
let column_validator = validate_ledes98bi_data_columns_greedy

let data_line_parser_greedy =
  let* f1 = data_field_parser in
  let* f2 = data_field_parser in
  let* f3 = data_field_parser in
  let* f4 = data_field_parser in
  let* f5 = data_field_parser in
  let* f6 = data_field_parser in
  let* f7 = data_field_parser in
  let* f8 = data_field_parser in
  let* f9 = data_field_parser in
  let* f10 = data_field_parser in
  let* f11 = data_field_parser in
  let* f12 = data_field_parser in
  let* f13 = data_field_parser in
  let* f14 = data_field_parser in
  let* f15 = data_field_parser in
  let* f16 = data_field_parser in
  let* f17 = data_field_parser in
  let* f18 = data_field_parser in
  let* f19 = data_field_parser in
  let* f20 = data_field_parser in
  let* f21 = data_field_parser in
  let* f22 = data_field_parser in
  let* f23 = data_field_parser in
  let* f24 = data_field_parser in
  let* f25 = data_field_parser in
  let* f26 = data_field_parser in
  let* f27 = data_field_parser in
  let* f28 = data_field_parser in
  let* f29 = data_field_parser in
  let* f30 = data_field_parser in
  let* f31 = data_field_parser in
  let* f32 = data_field_parser in
  let* f33 = data_field_parser in
  let* f34 = data_field_parser in
  let* f35 = data_field_parser in
  let* f36 = data_field_parser in
  let* f37 = data_field_parser in
  let* f38 = data_field_parser in
  let* f39 = data_field_parser in
  let* f40 = data_field_parser in
  let* f41 = data_field_parser in
  let* f42 = data_field_parser in
  let* f43 = data_field_parser in
  let* f44 = data_field_parser in
  let* f45 = data_field_parser in
  let* f46 = data_field_parser in
  let* f47 = data_field_parser in
  let* f48 = data_field_parser in
  let* f49 = data_field_parser in
  let* f50 = data_field_parser in
  let* f51 = data_field_parser in
  let* f52 = last_data_field_parser in
  return
    [
      f1;
      f2;
      f3;
      f4;
      f5;
      f6;
      f7;
      f8;
      f9;
      f10;
      f11;
      f12;
      f13;
      f14;
      f15;
      f16;
      f17;
      f18;
      f19;
      f20;
      f21;
      f22;
      f23;
      f24;
      f25;
      f26;
      f27;
      f28;
      f29;
      f30;
      f31;
      f32;
      f33;
      f34;
      f35;
      f36;
      f37;
      f38;
      f39;
      f40;
      f41;
      f42;
      f43;
      f44;
      f45;
      f46;
      f47;
      f48;
      f49;
      f50;
      f51;
      f52;
    ]

let data_fields_validator_greedy errors fields line_num =
  let f1 = get_index fields 1 in
  let f2 = get_index fields 2 in
  let f3 = get_index fields 3 in
  let f4 = get_index fields 4 in
  let f5 = get_index fields 5 in
  let f6 = get_index fields 6 in
  let f7 = get_index fields 7 in
  let f8 = get_index fields 8 in
  let f9 = get_index fields 9 in
  let f10 = get_index fields 10 in
  let f11 = get_index fields 11 in
  let f12 = get_index fields 12 in
  let f13 = get_index fields 13 in
  let f14 = get_index fields 14 in
  let f15 = get_index fields 15 in
  let f16 = get_index fields 16 in
  let f17 = get_index fields 17 in
  let f18 = get_index fields 18 in
  let f19 = get_index fields 19 in
  let f20 = get_index fields 20 in
  let f21 = get_index fields 21 in
  let f22 = get_index fields 22 in
  let f23 = get_index fields 23 in
  let f24 = get_index fields 24 in
  let f25 = get_index fields 25 in
  let f26 = get_index fields 26 in
  let f27 = get_index fields 27 in
  let f28 = get_index fields 28 in
  let f29 = get_index fields 29 in
  let f30 = get_index fields 30 in
  let f31 = get_index fields 31 in
  let f32 = get_index fields 32 in
  let f33 = get_index fields 33 in
  let f34 = get_index fields 34 in
  let f35 = get_index fields 35 in
  let f36 = get_index fields 36 in
  let f37 = get_index fields 37 in
  let f38 = get_index fields 38 in
  let f39 = get_index fields 39 in
  let f40 = get_index fields 40 in
  let f41 = get_index fields 41 in
  let f42 = get_index fields 42 in
  let f43 = get_index fields 43 in
  let f44 = get_index fields 44 in
  let f45 = get_index fields 45 in
  let f46 = get_index fields 46 in
  let f47 = get_index fields 47 in
  let f48 = get_index fields 48 in
  let f49 = get_index fields 49 in
  let f50 = get_index fields 50 in
  let f51 = get_index fields 51 in
  let f52 = get_index fields 52 in
  let errors =
    data_field_validate_greedy f1 errors ~line_num ~field_num:"1"
      ~field_name:"INVOICE_DATE" ~pred:is_valid_date ~required:true
  in
  let errors =
    data_field_validate_greedy f2 errors ~line_num ~field_num:"2"
      ~field_name:"INVOICE_NUMBER" ~pred:(is_valid_alphanumeric 20)
      ~required:true
  in
  let errors =
    data_field_validate_greedy f3 errors ~line_num ~field_num:"3"
      ~field_name:"CLIENT_ID" ~pred:(is_valid_char_field 20) ~required:true
  in
  let errors =
    data_field_validate_greedy f4 errors ~line_num ~field_num:"4"
      ~field_name:"LAW_FIRM_MATTER_ID" ~pred:(is_valid_char_field 20)
      ~required:true
  in
  let errors =
    data_field_validate_greedy f5 errors ~line_num ~field_num:"5"
      ~field_name:"INVOICE_TOTAL" ~pred:(is_valid_currency 12 4) ~required:true
  in
  let errors =
    data_field_validate_greedy f6 errors ~line_num ~field_num:"6"
      ~field_name:"BILLING_START_DATE" ~pred:is_valid_date ~required:true
  in
  let errors =
    data_field_validate_greedy f7 errors ~line_num ~field_num:"7"
      ~field_name:"BILLING_END_DATE" ~pred:is_valid_date ~required:true
  in
  let errors =
    data_field_validate_greedy f8 errors ~line_num ~field_num:"8"
      ~field_name:"INVOICE_DESCRIPTION" ~pred:(is_valid_kb_size 15)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f9 errors ~line_num ~field_num:"9"
      ~field_name:"LINE_ITEM_NUMBER" ~pred:(is_valid_char_field 20)
      ~required:true
  in
  let errors =
    data_field_validate_greedy f10 errors ~line_num ~field_num:"10"
      ~field_name:"EXP/FEE/INV_ADJ_TYPE"
      ~pred:(flip List.mem valid_exp_fee_inv_adi_type)
      ~required:true
  in
  let errors =
    data_field_validate_with_dep_greedy f11 errors ~line_num ~field_num:"11"
      ~field_name:"LINE_ITEM_NUMBER_OF_UNITS"
      ~pred:(is_valid_line_item_cost f10)
  in
  let errors =
    data_field_validate_greedy f12 errors ~line_num ~field_num:"12"
      ~field_name:"LINE_ITEM_ADJUSTMENT_AMOUNT" ~pred:(is_valid_currency 10 4)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f13 errors ~line_num ~field_num:"13"
      ~field_name:"LINE_ITEM_TOTAL" ~pred:(is_valid_currency 10 4)
      ~required:true
  in
  let errors =
    data_field_validate_greedy f14 errors ~line_num ~field_num:"14"
      ~field_name:"LINE_ITEM_DATE" ~pred:is_valid_date ~required:true
  in
  let errors =
    data_field_validate_greedy f15 errors ~line_num ~field_num:"15"
      ~field_name:"LINE_ITEM_TASK_CODE" ~pred:(is_valid_char_field 20)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f16 errors ~line_num ~field_num:"16"
      ~field_name:"LINE_ITEM_EXPENSE_CODE" ~pred:(is_valid_char_field 20)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f17 errors ~line_num ~field_num:"17"
      ~field_name:"LINE_ITEM_ACTIVITY_CODE" ~pred:(is_valid_char_field 20)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f18 errors ~line_num ~field_num:"18"
      ~field_name:"TIMEKEEPER_ID" ~pred:(is_valid_char_field 20) ~required:false
  in
  let errors =
    data_field_validate_greedy f19 errors ~line_num ~field_num:"19"
      ~field_name:"LINE_ITEM_DESCRIPTION" ~pred:(is_valid_kb_size 15)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f20 errors ~line_num ~field_num:"20"
      ~field_name:"LAW_FIRM_ID" ~pred:(is_valid_char_field 20) ~required:true
  in
  let errors =
    data_field_validate_with_dep_greedy f21 errors ~line_num ~field_num:"21"
      ~field_name:"LINE_ITEM_UNIT_COST"
      ~pred:(is_valid_line_item_cost f10)
  in
  let errors =
    data_field_validate_greedy f22 errors ~line_num ~field_num:"22"
      ~field_name:"TIMEKEEPER_NAME" ~pred:(is_valid_char_field 30)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f23 errors ~line_num ~field_num:"23"
      ~field_name:"TIMEKEEPER_CLASSIFICATION"
      ~pred:is_valid_timekeeper_classification ~required:false
  in
  let errors =
    data_field_validate_greedy f24 errors ~line_num ~field_num:"24"
      ~field_name:"CLIENT_MATTER_ID" ~pred:(is_valid_char_field 20)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f25 errors ~line_num ~field_num:"25"
      ~field_name:"PO_NUMBER"
      ~pred:(is_valid_alphanumeric 100)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f26 errors ~line_num ~field_num:"26"
      ~field_name:"CLIENT_TAX_ID" ~pred:(is_valid_char_field 20) ~required:true
  in
  let errors =
    data_field_validate_greedy f27 errors ~line_num ~field_num:"27"
      ~field_name:"MATTER_NAME" ~pred:(is_valid_char_field 255) ~required:true
  in
  let errors =
    data_field_validate_greedy f28 errors ~line_num ~field_num:"28"
      ~field_name:"INVOICE_TAX_TOTAL" ~pred:(is_valid_currency 12 4)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f29 errors ~line_num ~field_num:"29"
      ~field_name:"INVOICE_NET_TOTAL" ~pred:(is_valid_currency 12 4)
      ~required:true
  in
  let errors =
    data_field_validate_greedy f30 errors ~line_num ~field_num:"30"
      ~field_name:"INVOICE_CURRENCY" ~pred:is_valid_iso4217_code ~required:false
  in
  let errors =
    data_field_validate_with_dep_greedy f31 errors ~line_num ~field_num:"31"
      ~field_name:"TIMEKEEPER_LAST_NAME"
      ~pred:(is_valid_timekeep_name f22 f10)
  in
  let errors =
    data_field_validate_with_dep_greedy f32 errors ~line_num ~field_num:"32"
      ~field_name:"TIMEKEEPER_FIRST_NAME"
      ~pred:(is_valid_timekeep_name f22 f10)
  in
  let errors =
    data_field_validate_greedy f33 errors ~line_num ~field_num:"33"
      ~field_name:"ACCOUNT_TYPE" ~pred:is_valid_account_type ~required:false
  in
  let errors =
    data_field_validate_greedy f34 errors ~line_num ~field_num:"34"
      ~field_name:"LAW_FIRM_NAME" ~pred:(is_valid_char_field 60) ~required:false
  in
  let errors =
    data_field_validate_greedy f35 errors ~line_num ~field_num:"35"
      ~field_name:"LAW_FIRM_ADDRESS_1" ~pred:(is_valid_char_field 60)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f36 errors ~line_num ~field_num:"36"
      ~field_name:"LAW_FIRM_ADDRESS_2" ~pred:(is_valid_char_field 60)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f37 errors ~line_num ~field_num:"37"
      ~field_name:"LAW_FIRM_CITY" ~pred:(is_valid_char_field 40) ~required:false
  in
  let errors =
    data_field_validate_greedy f38 errors ~line_num ~field_num:"38"
      ~field_name:"LAW_FIRM_STATEorREGION" ~pred:(is_valid_char_field 40)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f39 errors ~line_num ~field_num:"39"
      ~field_name:"LAW_FIRM_POSTCODE" ~pred:(is_valid_char_field 20)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f40 errors ~line_num ~field_num:"40"
      ~field_name:"LAW_FIRM_COUNTRY" ~pred:(is_valid_char_field 3)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f41 errors ~line_num ~field_num:"41"
      ~field_name:"CLIENT_NAME" ~pred:(is_valid_char_field 60) ~required:false
  in
  let errors =
    data_field_validate_greedy f42 errors ~line_num ~field_num:"42"
      ~field_name:"CLIENT_ADDRESS_1" ~pred:(is_valid_char_field 60)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f43 errors ~line_num ~field_num:"43"
      ~field_name:"CLIENT_ADDRESS_2" ~pred:(is_valid_char_field 60)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f44 errors ~line_num ~field_num:"44"
      ~field_name:"CLIENT_CITY" ~pred:(is_valid_char_field 40) ~required:false
  in
  let errors =
    data_field_validate_greedy f45 errors ~line_num ~field_num:"45"
      ~field_name:"CLIENT_STATEorREGION" ~pred:(is_valid_char_field 40)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f46 errors ~line_num ~field_num:"46"
      ~field_name:"CLIENT_POSTCODE" ~pred:(is_valid_char_field 20)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f47 errors ~line_num ~field_num:"47"
      ~field_name:"CLIENT_COUNTRY" ~pred:(is_valid_char_field 3) ~required:false
  in
  let errors =
    data_field_validate_greedy f48 errors ~line_num ~field_num:"48"
      ~field_name:"LINE_ITEM_TAX_RATE" ~pred:is_valid_decimal ~required:false
  in
  let errors =
    data_field_validate_greedy f49 errors ~line_num ~field_num:"49"
      ~field_name:"LINE_ITEM_TAX_TOTAL" ~pred:(is_valid_currency 10 4)
      ~required:true
  in
  let errors =
    data_field_validate_greedy f50 errors ~line_num ~field_num:"50"
      ~field_name:"LINE_ITEM_TAX_TYPE" ~pred:(is_valid_char_field 20)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f51 errors ~line_num ~field_num:"51"
      ~field_name:"INVOICE_REPORTED_TAX_TOTAL" ~pred:(is_valid_currency 12 4)
      ~required:false
  in
  let errors =
    data_field_validate_greedy f52 errors ~line_num ~field_num:"52"
      ~field_name:"INVOICE_TAX_CURRENCY" ~pred:is_valid_iso4217_code
      ~required:false
  in
  List.rev errors

let parse_and_validate_greedy =
  parse_and_validate_greedy_inner ~header_validator ~column_validator
    ~data_line_parser_greedy ~data_fields_validator_greedy

let data_parser_eager =
  let* _ =
    data_field_parser_eager ~field_num:"1" ~field_name:"INVOICE_DATE"
      ~pred:is_valid_date ~required:true
  in
  let* _ =
    data_field_parser_eager ~field_num:"2" ~field_name:"INVOICE_NUMBER"
      ~pred:(is_valid_alphanumeric 50) ~required:true
  in
  let* _ =
    data_field_parser_eager ~field_num:"3" ~field_name:"CLIENT_ID"
      ~pred:(is_valid_char_field 20) ~required:true
  in
  let* _ =
    data_field_parser_eager ~field_num:"4" ~field_name:"LAW_FIRM_MATTER_ID"
      ~pred:(is_valid_char_field 20) ~required:true
  in
  let* _ =
    data_field_parser_eager ~field_num:"5" ~field_name:"INVOICE_TOTAL"
      ~pred:(is_valid_currency 12 4) ~required:true
  in
  let* _ =
    data_field_parser_eager ~field_num:"6" ~field_name:"BILLING_START_DATE"
      ~pred:is_valid_date ~required:true
  in
  let* _ =
    data_field_parser_eager ~field_num:"7" ~field_name:"BILLING_END_DATE"
      ~pred:is_valid_date ~required:true
  in
  let* _ =
    data_field_parser_eager ~field_num:"8" ~field_name:"INVOICE_DESCRIPTION"
      ~pred:(is_valid_kb_size 15) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"9" ~field_name:"LINE_ITEM_NUMBER"
      ~pred:(is_valid_char_field 20) ~required:true
  in
  let* f10 =
    data_field_parser_eager ~field_num:"10" ~field_name:"EXP/FEE/INV_ADJ_TYPE"
      ~pred:(flip List.mem valid_exp_fee_inv_adi_type)
      ~required:true
  in
  let* _ =
    data_field_parser_with_dep_eager ~field_num:"11"
      ~field_name:"LINE_ITEM_NUMBER_OF_UNITS"
      ~pred:(is_valid_line_item_cost f10)
  in
  let* _ =
    data_field_parser_eager ~field_num:"12"
      ~field_name:"LINE_ITEM_ADJUSTMENT_AMOUNT" ~pred:(is_valid_currency 10 4)
      ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"13" ~field_name:"LINE_ITEM_TOTAL"
      ~pred:(is_valid_currency 10 4) ~required:true
  in
  let* _ =
    data_field_parser_eager ~field_num:"14" ~field_name:"LINE_ITEM_DATE"
      ~pred:is_valid_date ~required:true
  in
  let* _ =
    data_field_parser_eager ~field_num:"15" ~field_name:"LINE_ITEM_TASK_CODE"
      ~pred:(is_valid_char_field 20) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"16" ~field_name:"LINE_ITEM_EXPENSE_CODE"
      ~pred:(is_valid_char_field 20) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"17"
      ~field_name:"LINE_ITEM_ACTIVITY_CODE" ~pred:(is_valid_char_field 20)
      ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"18" ~field_name:"TIMEKEEPER_ID"
      ~pred:(is_valid_char_field 20) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"19" ~field_name:"LINE_ITEM_DESCRIPTION"
      ~pred:(is_valid_kb_size 15) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"20" ~field_name:"LAW_FIRM_ID"
      ~pred:(is_valid_char_field 50) ~required:true
  in
  let* _ =
    data_field_parser_with_dep_eager ~field_num:"21"
      ~field_name:"LINE_ITEM_UNIT_COST"
      ~pred:(is_valid_line_item_cost f10)
  in
  let* f22 =
    data_field_parser_eager ~field_num:"22" ~field_name:"TIMEKEEPER_NAME"
      ~pred:(is_valid_char_field 30) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"23"
      ~field_name:"TIMEKEEPER_CLASSIFICATION"
      ~pred:is_valid_timekeeper_classification ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"24" ~field_name:"CLIENT_MATTER_ID"
      ~pred:(is_valid_char_field 20) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"25" ~field_name:"PO_NUMBER"
      ~pred:(is_valid_alphanumeric 100)
      ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"26" ~field_name:"CLIENT_TAX_ID"
      ~pred:(is_valid_char_field 20) ~required:true
  in
  let* _ =
    data_field_parser_eager ~field_num:"27" ~field_name:"MATTER_NAME"
      ~pred:(is_valid_char_field 255) ~required:true
  in
  let* _ =
    data_field_parser_eager ~field_num:"28" ~field_name:"INVOICE_TAX_TOTAL"
      ~pred:(is_valid_currency 12 4) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"29" ~field_name:"INVOICE_NET_TOTAL"
      ~pred:(is_valid_currency 12 4) ~required:true
  in
  let* _ =
    data_field_parser_eager ~field_num:"30" ~field_name:"INVOICE_CURRENCY"
      ~pred:is_valid_iso4217_code ~required:false
  in
  let* _ =
    data_field_parser_with_dep_eager ~field_num:"31"
      ~field_name:"TIMEKEEPER_LAST_NAME"
      ~pred:(is_valid_timekeep_name f22 f10)
  in
  let* _ =
    data_field_parser_with_dep_eager ~field_num:"32"
      ~field_name:"TIMEKEEPER_FIRST_NAME"
      ~pred:(is_valid_timekeep_name f22 f10)
  in
  let* _ =
    data_field_parser_eager ~field_num:"33" ~field_name:"ACCOUNT_TYPE"
      ~pred:is_valid_account_type ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"34" ~field_name:"LAW_FIRM_NAME"
      ~pred:(is_valid_char_field 60) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"35" ~field_name:"LAW_FIRM_ADDRESS_1"
      ~pred:(is_valid_char_field 60) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"36" ~field_name:"LAW_FIRM_ADDRESS_2"
      ~pred:(is_valid_char_field 60) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"37" ~field_name:"LAW_FIRM_CITY"
      ~pred:(is_valid_char_field 40) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"38" ~field_name:"LAW_FIRM_STATEorREGION"
      ~pred:(is_valid_char_field 40) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"39" ~field_name:"LAW_FIRM_POSTCODE"
      ~pred:(is_valid_char_field 20) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"40" ~field_name:"LAW_FIRM_COUNTRY"
      ~pred:(is_valid_char_field 3) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"41" ~field_name:"CLIENT_NAME"
      ~pred:(is_valid_char_field 60) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"42" ~field_name:"CLIENT_ADDRESS_1"
      ~pred:(is_valid_char_field 60) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"43" ~field_name:"CLIENT_ADDRESS_2"
      ~pred:(is_valid_char_field 60) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"44" ~field_name:"CLIENT_CITY"
      ~pred:(is_valid_char_field 40) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"45" ~field_name:"CLIENT_STATEorREGION"
      ~pred:(is_valid_char_field 40) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"46" ~field_name:"CLIENT_POSTCODE"
      ~pred:(is_valid_char_field 20) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"47" ~field_name:"CLIENT_COUNTRY"
      ~pred:(is_valid_char_field 3) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"48" ~field_name:"LINE_ITEM_TAX_RATE"
      ~pred:is_valid_decimal ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"49" ~field_name:"LINE_ITEM_TAX_TOTAL"
      ~pred:(is_valid_currency 10 4) ~required:true
  in
  let* _ =
    data_field_parser_eager ~field_num:"50" ~field_name:"LINE_ITEM_TAX_TYPE"
      ~pred:(is_valid_char_field 20) ~required:false
  in
  let* _ =
    data_field_parser_eager ~field_num:"51"
      ~field_name:"INVOICE_REPORTED_TAX_TOTAL" ~pred:(is_valid_currency 12 4)
      ~required:false
  in
  last_data_field_parser_eager ~field_num:"52"
    ~field_name:"INVOICE_TAX_CURRENCY" ~pred:is_valid_iso4217_code
    ~required:false

let parse_and_validate_eager lines =
  parse_and_validate_eager_inner lines
    ~header_parser_eager:ledes98bi_header_parser
    ~column_parser_eager:ledes98bi_column_parser ~data_parser:data_parser_eager
