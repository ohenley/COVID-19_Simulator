with Ada.Directories; use Ada.Directories;

procedure body platform is

begin

   function get_ui_specification_filepath return string is
   begin
      return "../../../../../../src/form/covidsim_form.ui";
   end;

   function get_covid_raw_data_filepath return string is
   begin
      return "../../../../../../deps/xph_covid19/data/covid19.csv";
   end;

end;
