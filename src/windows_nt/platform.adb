with Ada.Directories; use Ada.Directories;

package body platform is

   function get_ui_specification_filepath return string is
   begin
      return Current_Directory & "/../../../../../src/form/covidsim_form.ui";
   end;

   function get_covid_raw_data_filepath return string is
   begin
      return Current_Directory & "/../../../../../deps/xph_covid19/data/covid19.csv";
   end;

end;
