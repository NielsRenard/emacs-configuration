;;-*-coding: utf-8;-*-
(define-abbrev-table 'cperl-mode-abbrev-table
  '(
    ("=head1" "=head1" cperl-electric-pod :count 0)
    ("=head2" "=head2" cperl-electric-pod :count 0)
    ("=over" "=over" cperl-electric-pod :count 0)
    ("=pod" "=pod" cperl-electric-pod :count 0)
    ("continue" "continue" cperl-electric-else :count 0)
    ("do" "do" cperl-electric-keyword :count 0)
    ("else" "else" cperl-electric-else :count 0)
    ("elsif" "elsif" cperl-electric-keyword :count 0)
    ("for" "for" cperl-electric-keyword :count 0)
    ("foreach" "foreach" cperl-electric-keyword :count 0)
    ("foreachmy" "foreachmy" cperl-electric-keyword :count 0)
    ("formy" "formy" cperl-electric-keyword :count 0)
    ("head1" "head1" cperl-electric-pod :count 0)
    ("head2" "head2" cperl-electric-pod :count 0)
    ("if" "if" cperl-electric-keyword :count 0)
    ("over" "over" cperl-electric-pod :count 0)
    ("pod" "pod" cperl-electric-pod :count 0)
    ("unless" "unless" cperl-electric-keyword :count 0)
    ("until" "until" cperl-electric-keyword :count 0)
    ("while" "while" cperl-electric-keyword :count 0)
   ))

(define-abbrev-table 'global-abbrev-table
  '(
    ("langp" "{-# LANGUAGE  #-}" nil :count 8)
    ("langprag" "{-# LANGUAGE  #-}" nil :count 8)
    ("lp os" "{-# LANGUAGE OverloadedStrings #-}" nil :count 6)
    ("lp qq" "{-# LANGUAGE QuasiQuotes #-}" nil :count 6)
    ("lpos" "{-# LANGUAGE OverloadedStrings #-}" nil :count 7)
    ("lpqq" "{-# LANGUAGE QuasiQuotes #-}" nil :count 7)
    ("lprag" "{-# LANGUAGE  #-}" nil :count 8)
    ("prl" "println!();" nil :count 20)
    ("prpr" "println!(\"{:?}\", a);" nil :count 85)
    ("prt" "println!(\"☬\");" nil :count 70)
    ("prxy" "println!(\"x: {:?}, y: {:?}\", x, y);" nil :count 15)
   ))

(define-abbrev-table 'java-mode-abbrev-table
  '(
    ("newclass" "public class MyClass {
  public static void main(String[] args) {
    myMethod(\"☃\");
  }
  public static void myMethod (String s){
    System.out.println(s);
  }
}" nil :count 7)
    ("psvm" "  public static void main(String[] args) {
    System.out.println(\"☃\");
  }" nil :count 14)
    ("sout" "System.out.println(\"☃\")" nil :count 9)
   ))

