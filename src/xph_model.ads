with Qt;  use Qt;

with xph_covid19; use xph_covid19;

package xph_model is

   --  procedure set_inputs(country_val : country;
   --                       start_day_index_val : integer;
   --                       end_day_index_val : integer;
   --                       steps_val : integer;
   --                       minimize_by_density_val : boolean;
   --                       zoom_factor_val : float;
   --                       minimal_improvement_percentage_val : float;
   --                       fore_term_val : integer;
   --                       bend_percent_val : float);

   procedure init_model (form_widget: QWidgetH; chart_widget : QChartH);


   procedure slot_compute_xph;
   pragma Convention (C, slot_compute_xph);

   procedure slot_change_country_choice (country_name : QStringH);
   pragma Convention (C, slot_change_country_choice);

   procedure slot_start_date_changed (date: QDateH);
   pragma Convention (C, slot_start_date_changed);

   procedure slot_end_date_changed (date: QDateH);
   pragma Convention (C, slot_end_date_changed);

   procedure slot_change_forecast_days (foracast_days_val: Integer);
   pragma Convention (C, slot_change_forecast_days);

   procedure slot_change_refine_search (state: integer);
   pragma Convention (C, slot_change_refine_search);


end xph_model;
