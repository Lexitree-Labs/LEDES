open Ledes

let read_lines filename =
  let lines = ref [] in
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      lines := line :: !lines
    done;
    []
  with End_of_file ->
    close_in ic;
    List.rev !lines

let%expect_test "ledes1998b_greedy_good" =
  let lines = read_lines "data/ledes1998b_good.txt" in
  let err = Ledes1998b.parse_and_validate_greedy lines in
  print_endline @@ String.concat "\n" err;
  [%expect {||}]

let%expect_test "ledes1998b_eager_good" =
  let lines = read_lines "data/ledes1998b_good.txt" in
  Ledes1998b.parse_and_validate_eager lines;
  [%expect {||}]

let%expect_test "ledes1998b_greedy_bad" =
  let lines = read_lines "data/ledes1998b_greedy_bad.txt" in
  let err = Ledes1998b.parse_and_validate_greedy lines in
  print_endline @@ String.concat "\n" err;
  [%expect
    {|
    Line 1: Invalid or missing LEDES1998B header
    Line 2: Invalid or missing LEDES1998B column definition
    Line 3 : At field 1 : Invalid INVOICE_DATE
    Line 3 : At field 2 : Invalid INVOICE_NUMBER
    Line 3 : At field 3 : Invalid CLIENT_ID
    Line 3 : At field 4 : Invalid LAW_FIRM_MATTER_ID
    Line 3 : At field 5 : Invalid INVOICE_TOTAL
    Line 3 : At field 6 : Invalid BILLING_START_DATE
    Line 3 : At field 7 : Invalid BILLING_END_DATE
    Line 3 : At field 9 : Invalid LINE_ITEM_NUMBER
    Line 3 : At field 10 : Invalid EXP/FEE/INV_ADJ_TYPE
    Line 3 : At field 12 : Invalid LINE_ITEM_ADJUSTMENT_AMOUNT
    Line 3 : At field 13 : Invalid LINE_ITEM_TOTAL
    Line 3 : At field 14 : Invalid LINE_ITEM_DATE
    Line 3 : At field 15 : Invalid LINE_ITEM_TASK_CODE
    Line 3 : At field 16 : Invalid LINE_ITEM_EXPENSE_CODE
    Line 3 : At field 17 : Invalid LINE_ITEM_ACTIVITY_CODE
    Line 3 : At field 18 : Invalid TIMEKEEPER_ID
    Line 3 : At field 20 : Invalid LAW_FIRM_ID
    Line 3 : At field 21 : Invalid LINE_ITEM_UNIT_COST
    Line 3 : At field 22 : Invalid TIMEKEEPER_NAME
    Line 3 : At field 23 : Invalid TIMEKEEPER_CLASSIFICATION
    Line 3 : At field 24 : Invalid CLIENT_MATTER_ID
    Line 4 : At field 1 : Invalid INVOICE_DATE
    Line 4 : At field 2 : Invalid INVOICE_NUMBER
    Line 4 : At field 3 : Invalid CLIENT_ID
    Line 4 : At field 4 : Invalid LAW_FIRM_MATTER_ID
    Line 4 : At field 5 : Invalid INVOICE_TOTAL
    Line 4 : At field 6 : Invalid BILLING_START_DATE
    Line 4 : At field 7 : Invalid BILLING_END_DATE
    Line 4 : At field 9 : Invalid LINE_ITEM_NUMBER
    Line 4 : At field 10 : Invalid EXP/FEE/INV_ADJ_TYPE
    Line 4 : At field 12 : Invalid LINE_ITEM_ADJUSTMENT_AMOUNT
    Line 4 : At field 13 : Invalid LINE_ITEM_TOTAL
    Line 4 : At field 14 : Invalid LINE_ITEM_DATE
    Line 4 : At field 15 : Invalid LINE_ITEM_TASK_CODE
    Line 4 : At field 16 : Invalid LINE_ITEM_EXPENSE_CODE
    Line 4 : At field 17 : Invalid LINE_ITEM_ACTIVITY_CODE
    Line 4 : At field 18 : Invalid TIMEKEEPER_ID
    Line 4 : At field 20 : Invalid LAW_FIRM_ID
    Line 4 : At field 21 : Invalid LINE_ITEM_UNIT_COST
    Line 4 : At field 22 : Invalid TIMEKEEPER_NAME
    Line 4 : At field 23 : Invalid TIMEKEEPER_CLASSIFICATION
    Line 4 : At field 24 : Invalid CLIENT_MATTER_ID |}]

let%expect_test "ledes1998b_eager_bad" =
  let lines = read_lines "data/ledes1998b_eager_bad.txt" in
  Ledes1998b.parse_and_validate_eager lines;
  [%expect {| Line 3 : At field 21 : Invalid LINE_ITEM_UNIT_COST |}]

let%expect_test "ledes98bi_greedy_good" =
  let lines = read_lines "data/ledes98bi_good.txt" in
  let err = Ledes98bi.parse_and_validate_greedy lines in
  print_endline @@ String.concat "\n" err;
  [%expect {||}]

let%expect_test "ledes98bi_eager_good" =
  let lines = read_lines "data/ledes98bi_good.txt" in
  Ledes98bi.parse_and_validate_eager lines;
  [%expect {||}]

let%expect_test "ledes98bi_greedy_bad" =
  let lines = read_lines "data/ledes98bi_greedy_bad.txt" in
  let err = Ledes98bi.parse_and_validate_greedy lines in
  print_endline @@ String.concat "\n" err;
  [%expect
    {|
    Line 1: Invalid or missing LEDES98BI header
    Line 2: Invalid or missing LEDES98BI column definition
    Line 3 : At field 1 : Invalid INVOICE_DATE
    Line 3 : At field 2 : Invalid INVOICE_NUMBER
    Line 3 : At field 3 : Invalid CLIENT_ID
    Line 3 : At field 4 : Invalid LAW_FIRM_MATTER_ID
    Line 3 : At field 5 : Invalid INVOICE_TOTAL
    Line 3 : At field 6 : Invalid BILLING_START_DATE
    Line 3 : At field 7 : Invalid BILLING_END_DATE
    Line 3 : At field 9 : Invalid LINE_ITEM_NUMBER
    Line 3 : At field 10 : Invalid EXP/FEE/INV_ADJ_TYPE
    Line 3 : At field 12 : Invalid LINE_ITEM_ADJUSTMENT_AMOUNT
    Line 3 : At field 13 : Invalid LINE_ITEM_TOTAL
    Line 3 : At field 14 : Invalid LINE_ITEM_DATE
    Line 3 : At field 15 : Invalid LINE_ITEM_TASK_CODE
    Line 3 : At field 16 : Invalid LINE_ITEM_EXPENSE_CODE
    Line 3 : At field 17 : Invalid LINE_ITEM_ACTIVITY_CODE
    Line 3 : At field 18 : Invalid TIMEKEEPER_ID
    Line 3 : At field 20 : Invalid LAW_FIRM_ID
    Line 3 : At field 22 : Invalid TIMEKEEPER_NAME
    Line 3 : At field 23 : Invalid TIMEKEEPER_CLASSIFICATION
    Line 3 : At field 24 : Invalid CLIENT_MATTER_ID
    Line 3 : At field 25 : Invalid PO_NUMBER
    Line 3 : At field 26 : Invalid CLIENT_TAX_ID
    Line 3 : At field 27 : Invalid MATTER_NAME
    Line 3 : At field 28 : Invalid INVOICE_TAX_TOTAL
    Line 3 : At field 29 : Invalid INVOICE_NET_TOTAL
    Line 3 : At field 30 : Invalid INVOICE_CURRENCY
    Line 3 : At field 31 : Invalid TIMEKEEPER_LAST_NAME
    Line 3 : At field 32 : Invalid TIMEKEEPER_FIRST_NAME
    Line 3 : At field 33 : Invalid ACCOUNT_TYPE
    Line 3 : At field 34 : Invalid LAW_FIRM_NAME
    Line 3 : At field 35 : Invalid LAW_FIRM_ADDRESS_1
    Line 3 : At field 36 : Invalid LAW_FIRM_ADDRESS_2
    Line 3 : At field 37 : Invalid LAW_FIRM_CITY
    Line 3 : At field 38 : Invalid LAW_FIRM_STATEorREGION
    Line 3 : At field 39 : Invalid LAW_FIRM_POSTCODE
    Line 3 : At field 40 : Invalid LAW_FIRM_COUNTRY
    Line 3 : At field 41 : Invalid CLIENT_NAME
    Line 3 : At field 42 : Invalid CLIENT_ADDRESS_1
    Line 3 : At field 43 : Invalid CLIENT_ADDRESS_2
    Line 3 : At field 44 : Invalid CLIENT_CITY
    Line 3 : At field 45 : Invalid CLIENT_STATEorREGION
    Line 3 : At field 46 : Invalid CLIENT_POSTCODE
    Line 3 : At field 47 : Invalid CLIENT_COUNTRY
    Line 3 : At field 48 : Invalid LINE_ITEM_TAX_RATE
    Line 3 : At field 49 : Invalid LINE_ITEM_TAX_TOTAL
    Line 3 : At field 50 : Invalid LINE_ITEM_TAX_TYPE
    Line 3 : At field 51 : Invalid INVOICE_REPORTED_TAX_TOTAL
    Line 3 : At field 52 : Invalid INVOICE_TAX_CURRENCY
    Line 4 : At field 1 : Invalid INVOICE_DATE
    Line 4 : At field 2 : Invalid INVOICE_NUMBER
    Line 4 : At field 3 : Invalid CLIENT_ID
    Line 4 : At field 4 : Invalid LAW_FIRM_MATTER_ID
    Line 4 : At field 5 : Invalid INVOICE_TOTAL
    Line 4 : At field 6 : Invalid BILLING_START_DATE
    Line 4 : At field 7 : Invalid BILLING_END_DATE
    Line 4 : At field 9 : Invalid LINE_ITEM_NUMBER
    Line 4 : At field 10 : Invalid EXP/FEE/INV_ADJ_TYPE
    Line 4 : At field 12 : Invalid LINE_ITEM_ADJUSTMENT_AMOUNT
    Line 4 : At field 13 : Invalid LINE_ITEM_TOTAL
    Line 4 : At field 14 : Invalid LINE_ITEM_DATE
    Line 4 : At field 15 : Invalid LINE_ITEM_TASK_CODE
    Line 4 : At field 16 : Invalid LINE_ITEM_EXPENSE_CODE
    Line 4 : At field 17 : Invalid LINE_ITEM_ACTIVITY_CODE
    Line 4 : At field 18 : Invalid TIMEKEEPER_ID
    Line 4 : At field 20 : Invalid LAW_FIRM_ID
    Line 4 : At field 22 : Invalid TIMEKEEPER_NAME
    Line 4 : At field 23 : Invalid TIMEKEEPER_CLASSIFICATION
    Line 4 : At field 24 : Invalid CLIENT_MATTER_ID
    Line 4 : At field 25 : Invalid PO_NUMBER
    Line 4 : At field 26 : Invalid CLIENT_TAX_ID
    Line 4 : At field 27 : Invalid MATTER_NAME
    Line 4 : At field 28 : Invalid INVOICE_TAX_TOTAL
    Line 4 : At field 29 : Invalid INVOICE_NET_TOTAL
    Line 4 : At field 30 : Invalid INVOICE_CURRENCY
    Line 4 : At field 31 : Invalid TIMEKEEPER_LAST_NAME
    Line 4 : At field 32 : Invalid TIMEKEEPER_FIRST_NAME
    Line 4 : At field 33 : Invalid ACCOUNT_TYPE
    Line 4 : At field 34 : Invalid LAW_FIRM_NAME
    Line 4 : At field 35 : Invalid LAW_FIRM_ADDRESS_1
    Line 4 : At field 36 : Invalid LAW_FIRM_ADDRESS_2
    Line 4 : At field 37 : Invalid LAW_FIRM_CITY
    Line 4 : At field 38 : Invalid LAW_FIRM_STATEorREGION
    Line 4 : At field 39 : Invalid LAW_FIRM_POSTCODE
    Line 4 : At field 40 : Invalid LAW_FIRM_COUNTRY
    Line 4 : At field 41 : Invalid CLIENT_NAME
    Line 4 : At field 42 : Invalid CLIENT_ADDRESS_1
    Line 4 : At field 43 : Invalid CLIENT_ADDRESS_2
    Line 4 : At field 44 : Invalid CLIENT_CITY
    Line 4 : At field 45 : Invalid CLIENT_STATEorREGION
    Line 4 : At field 46 : Invalid CLIENT_POSTCODE
    Line 4 : At field 47 : Invalid CLIENT_COUNTRY
    Line 4 : At field 48 : Invalid LINE_ITEM_TAX_RATE
    Line 4 : At field 49 : Invalid LINE_ITEM_TAX_TOTAL
    Line 4 : At field 50 : Invalid LINE_ITEM_TAX_TYPE
    Line 4 : At field 51 : Invalid INVOICE_REPORTED_TAX_TOTAL
    Line 4 : At field 52 : Invalid INVOICE_TAX_CURRENCY |}]

let%expect_test "ledes98bi_eager_bad" =
  let lines = read_lines "data/ledes98bi_eager_bad.txt" in
  Ledes98bi.parse_and_validate_eager lines;
  [%expect {| Line 3 : At field 31 : Invalid TIMEKEEPER_LAST_NAME |}]

let%expect_test "ledes1998b_greedy_line_test" =
  let line =
    "LEDES1998BB[]\n\
     INVOIC_DATE|INVOICE_NUMBER|CLIENT_ID|LAW_FIRM_MATTER_ID|INVOICE_TOTAL|BILLING_START_DATE|BILLING_END_DATE|INVOICE_DESCRIPTION|LINE_ITEM_NUMBER|EXP/FEE/INV_ADJ_TYPE|LINE_ITEM_NUMBER_OF_UNITS|LINE_ITEM_ADJUSTMENT_AMOUNT|LINE_ITEM_TOTAL|LINE_ITEM_DATE|LINE_ITEM_TASK_CODE|LINE_ITEM_EXPENSE_CODE|LINE_ITEM_ACTIVITY_CODE|TIMEKEEPER_ID|LINE_ITEM_DESCRIPTION|LAW_FIRM_ID|LINE_ITEM_UNIT_COST|TIMEKEEPER_NAME|TIMEKEEPER_CLASSIFICATION|CLIENT_MATTER_ID[]\n\
     19990299|96543965439654396543123|96543965439654396543123||1250.123123|19990199|19990199|Monthly \
     Retainer|96543965439654396543123|A|1|1250.12311|1250.123123|19990199|96543965439654396543123|96543965439654396543123|96543965439654396543123|96543965439654396543123|Monthly \
     Retainer \
     Fee|96543965439654396543123||9654396543965439654312396543965439654396543123|ABCDE|96543965439654396543123[]\n\
     19990299|96543965439654396543123|96543965439654396543123||1250.123123|19990199|19990199|Monthly \
     Retainer|96543965439654396543123|A|1|1250.12311|1250.123123|19990199|96543965439654396543123|96543965439654396543123|96543965439654396543123|96543965439654396543123|Monthly \
     Retainer \
     Fee|96543965439654396543123||9654396543965439654312396543965439654396543123|ABCDE|96543965439654396543123[]"
  in
  let lines = String.split_on_char '\n' line in
  let err = Ledes1998b.parse_and_validate_greedy lines in
  print_endline @@ String.concat "\n" err;
  [%expect
    {|
    Line 1: Invalid or missing LEDES1998B header
    Line 2: Invalid or missing LEDES1998B column definition
    Line 3 : At field 1 : Invalid INVOICE_DATE
    Line 3 : At field 2 : Invalid INVOICE_NUMBER
    Line 3 : At field 3 : Invalid CLIENT_ID
    Line 3 : At field 4 : Invalid LAW_FIRM_MATTER_ID
    Line 3 : At field 5 : Invalid INVOICE_TOTAL
    Line 3 : At field 6 : Invalid BILLING_START_DATE
    Line 3 : At field 7 : Invalid BILLING_END_DATE
    Line 3 : At field 9 : Invalid LINE_ITEM_NUMBER
    Line 3 : At field 10 : Invalid EXP/FEE/INV_ADJ_TYPE
    Line 3 : At field 12 : Invalid LINE_ITEM_ADJUSTMENT_AMOUNT
    Line 3 : At field 13 : Invalid LINE_ITEM_TOTAL
    Line 3 : At field 14 : Invalid LINE_ITEM_DATE
    Line 3 : At field 15 : Invalid LINE_ITEM_TASK_CODE
    Line 3 : At field 16 : Invalid LINE_ITEM_EXPENSE_CODE
    Line 3 : At field 17 : Invalid LINE_ITEM_ACTIVITY_CODE
    Line 3 : At field 18 : Invalid TIMEKEEPER_ID
    Line 3 : At field 20 : Invalid LAW_FIRM_ID
    Line 3 : At field 21 : Invalid LINE_ITEM_UNIT_COST
    Line 3 : At field 22 : Invalid TIMEKEEPER_NAME
    Line 3 : At field 23 : Invalid TIMEKEEPER_CLASSIFICATION
    Line 3 : At field 24 : Invalid CLIENT_MATTER_ID
    Line 4 : At field 1 : Invalid INVOICE_DATE
    Line 4 : At field 2 : Invalid INVOICE_NUMBER
    Line 4 : At field 3 : Invalid CLIENT_ID
    Line 4 : At field 4 : Invalid LAW_FIRM_MATTER_ID
    Line 4 : At field 5 : Invalid INVOICE_TOTAL
    Line 4 : At field 6 : Invalid BILLING_START_DATE
    Line 4 : At field 7 : Invalid BILLING_END_DATE
    Line 4 : At field 9 : Invalid LINE_ITEM_NUMBER
    Line 4 : At field 10 : Invalid EXP/FEE/INV_ADJ_TYPE
    Line 4 : At field 12 : Invalid LINE_ITEM_ADJUSTMENT_AMOUNT
    Line 4 : At field 13 : Invalid LINE_ITEM_TOTAL
    Line 4 : At field 14 : Invalid LINE_ITEM_DATE
    Line 4 : At field 15 : Invalid LINE_ITEM_TASK_CODE
    Line 4 : At field 16 : Invalid LINE_ITEM_EXPENSE_CODE
    Line 4 : At field 17 : Invalid LINE_ITEM_ACTIVITY_CODE
    Line 4 : At field 18 : Invalid TIMEKEEPER_ID
    Line 4 : At field 20 : Invalid LAW_FIRM_ID
    Line 4 : At field 21 : Invalid LINE_ITEM_UNIT_COST
    Line 4 : At field 22 : Invalid TIMEKEEPER_NAME
    Line 4 : At field 23 : Invalid TIMEKEEPER_CLASSIFICATION
    Line 4 : At field 24 : Invalid CLIENT_MATTER_ID |}]
