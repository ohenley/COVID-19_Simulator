with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;

with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;

with xph_covid19; use xph_covid19;
with xph_covid19.data; use xph_covid19.data;
with xph_covid19.serialization; use xph_covid19.serialization;
with xph_covid19.utilities; use xph_covid19.utilities;

with Qt.QObject; use Qt.QObject;
with Qt.QWidget; use Qt.QWidget;
with Qt.QString; use Qt.QString;
with Qt.QComboBox; use Qt.QComboBox;
with Qt.QStringList; use Qt.QStringList;
with Qt.QXYSeries; use Qt.QXYSeries;
with Qt.QChart; use Qt.QChart;
with Qt.QLineSeries; use Qt.QLineSeries;
with Qt.QDateTime; use Qt.QDateTime;
with Qt.QVariant;    use Qt.QVariant;
with Qt.QValueAxis; use Qt.QValueAxis;
with Qt.QObjectList;    use Qt.QObjectList;
with Qt.QSpinBox; use Qt.QSpinBox;
with Qt.QAreaSeries; use Qt.QAreaSeries;
with Qt.QColor; use Qt.QColor;
with Qt.QPoint; use Qt.QPoint;
with Qt.QTimer; use Qt.QTimer;
with Qt.QCheckBox; use Qt.QCheckBox;
with Qt.QApplication; use Qt.QApplication;
with Qt.QNamespace; use Qt.QNamespace;
with Qt.QCursor; use Qt.QCursor;

with Qt.QScatterSeries;      use Qt.QScatterSeries;


with Qt.QPen;            use Qt.QPen;
with Qt.QBrush;          use Qt.QBrush;
with Qt.QPainter;        use Qt.QPainter;
with Qt.QGradient;       use Qt.QGradient;
with Qt.QAbstractSeries; use Qt.QAbstractSeries;

-- TODO:
-- 1. implement progress bar, feedback and cancel.
-- 2. proper legend
-- 3. redraw ui when changing country

package body xph_model is

   data_filename : string := "../../../../../../deps/xph_covid19/data/covid19.csv";

   bend : integer := 1; -- array index for bend in forecast_ce
   first_case : integer := 6; -- arr. index in forecast_ce when 1'st case appeared

   country_entries : country_entries_vector.vector;
   country_forecast_entries : country_entries_vector.vector;

   chart : QChartH;
   form : QWidgetH;

   country_choice : QComboBoxH;
   start_date_value : QDateEditH;
   end_date_value : QDateEditH;
   steps_value : QSpinBoxH;
   forecast_days_value : QSpinBoxH;
   minimize_by_density : QCheckBoxH;

   refine_search_group : QGroupBoxH;
   zoom_factor_value : QDoubleSpinBoxH;
   minimal_improvement_percentage_value : QDoubleSpinBoxH;

   procedure init_country_choices is
      countries : QStringListH  := QStringList_create;
      disable : integer := 0;
   begin
      for c in country loop
         QComboBox_addItem (country_choice, s2qs(all_countries(c).name));

         declare
            raw_ce : country_entries_array := get_country_data (data_filename, c);
         begin
            if raw_ce'length < 5 then
               QComboBox_setItemData (country_choice, country'pos(c),  QVariant_create(disable), QtUserRole-1);
            end if;
         end;

      end loop;
   end;



   procedure update_forecast_range_chart is
      axes : QObjectListH;
      forecast_last : qreal := qreal (QSpinBox_value (forecast_days_value));
   begin
      axes := QChart_axes(chart);
      QValueAxis_setRange(QAxisH(QObjectList_at(axes, 0)), 0.0, forecast_last);
   end;


   procedure update_chart is
      current_index : integer := QComboBox_currentIndex (country_choice);
      c : country := country'val(current_index);

      start_date : QDateH := QDateTimeEdit_date (start_date_value);
      start_time : Time := time_of (QDate_year (start_date), QDate_month (start_date), QDate_day (start_date));

      end_date : QDateH := QDateTimeEdit_date (end_date_value);
      end_time : Time := time_of (QDate_year (end_date), QDate_month (end_date), QDate_day (end_date));

      country_ground_truth_first : QSeriesH;
      country_ground_truth_date_range : QSeriesH;
      country_ground_truth_last : QSeriesH;

      country_forecast : QSeriesH;

      procedure init_series is
         pen_first : QPenH := QPen_create(QColor_create(0,255,0));
         pen_date_range : QPenH := QPen_create(QColor_create(255,0,0));
         pen_last : QPenH := QPen_create(QColor_create(0,255,0));
         pen_forecast: QPenH := QPen_create(QColor_create(255,255,0));
      begin
         country_ground_truth_first := QLineSeries_create;
         QPen_setWidth(pen_first, 3);
         QAreaSeries_setPen(country_ground_truth_first, pen_first);

         country_ground_truth_date_range := QLineSeries_create;
         QPen_setWidth(pen_date_range, 3);
         QAreaSeries_setPen(country_ground_truth_date_range, pen_date_range);

         country_ground_truth_last := QLineSeries_create;
         QPen_setWidth(pen_last, 3);
         QAreaSeries_setPen(country_ground_truth_last, pen_last);

         country_forecast := QLineSeries_create;
         QPen_setWidth(pen_forecast, 3);
         QAreaSeries_setPen(country_forecast, pen_forecast);
      end;

   begin
      QChart_removeAllSeries (chart);

      QChart_setTitle (chart, s2qs (all_countries(c).name));

      init_series;

      -- ground truths
      for e in country_entries.first_index .. country_entries.last_index loop

         if ada.calendar.arithmetic."-" (start_time, country_entries(e).date) > 0 then
            QXYSeries_append (country_ground_truth_first, qreal(e), qreal(country_entries(e).cumulative_cases));
         elsif ada.calendar.arithmetic."-" (country_entries(e).date, end_time) > 0 then
            QXYSeries_append (country_ground_truth_last, qreal(e), qreal(country_entries(e).cumulative_cases));
         else
            QXYSeries_append (country_ground_truth_date_range, qreal(e), qreal(country_entries(e).cumulative_cases));
         end if;

      end loop;

      -- forecast
      for f in country_forecast_entries.first_index .. country_forecast_entries.last_index loop
         QXYSeries_append (country_forecast, qreal(f), qreal(country_forecast_entries(f).cumulative_cases_simulated));
      end loop;


      QChart_addSeries (chart, country_ground_truth_first);
      QChart_addSeries (chart, country_ground_truth_date_range);
      QChart_addSeries (chart, country_ground_truth_last);
      QChart_addSeries (chart, country_forecast);

      QChart_createDefaultAxes (chart);

   end;

   procedure reset_chart is
   begin
      country_forecast_entries.clear;
      update_chart;
      update_forecast_range_chart;
   end;

   procedure init_model (form_widget: QWidgetH; chart_widget : QChartH) is
   begin
      form := form_widget;
      chart := chart_widget;

      country_choice := QComboBoxH (QObject_findChild (QObjectH (form), s2qs ("country_choice")));
      start_date_value := QDateEditH (QObject_findChild (QObjectH (form), s2qs ("start_date_value")));
      end_date_value := QDateEditH (QObject_findChild (QObjectH (form), s2qs ("end_date_value")));

      steps_value := QSpinBoxH (QObject_findChild (QObjectH (form), s2qs ("steps_value")));
      forecast_days_value := QSpinBoxH (QObject_findChild (QObjectH (form), s2qs ("forecast_days_value")));
      minimize_by_density := QCheckBoxH (QObject_findChild (QObjectH (form), s2qs ("minimize_by_density")));

      refine_search_group := QGroupBoxH (QObject_findChild (QObjectH (form), s2qs ("refine_search_group")));
      zoom_factor_value := QDoubleSpinBoxH (QObject_findChild (QObjectH (form), s2qs ("zoom_factor_value")));
      minimal_improvement_percentage_value := QDoubleSpinBoxH (QObject_findChild (QObjectH (form), s2qs ("minimal_improvement_percentage_value")));

      init_country_choices;

   end;

   procedure set_initial_date_limits is

      function find_first_case_date return integer is
      begin
         for i in country_entries.first_index .. country_entries.last_index loop
            if country_entries.element (i).cumulative_cases > 0.0 then
               return i;
            end if;
         end loop;
         return -1;
      end;

      start_time : Time := country_entries.element(find_first_case_date).date;
      start_date : QDateH := QDate_create (Ada.Calendar.Formatting.year (start_time), Ada.Calendar.Formatting.month (start_time), Ada.Calendar.Formatting.day (start_time));
      end_time : Time := country_entries.last_element.date;
      end_date : QDateH := QDate_create (Ada.Calendar.Formatting.year (end_time), Ada.Calendar.Formatting.month (end_time), Ada.Calendar.Formatting.day (end_time));
   begin
      QDateTimeEdit_setMinimumDate (start_date_value, start_date);
      QDateTimeEdit_setDate (start_date_value, start_date);

      QDateTimeEdit_setDate (end_date_value, end_date);
      QDateTimeEdit_setMaximumDate (end_date_value, end_date);
   end;



   procedure update_min_max_date_limits is

      function get_maximum_start_time(end_time : time) return Time is
      begin
         return ada.calendar.arithmetic."-" (end_time, 5);
      end;

      function get_minimum_end_time (start_time : time) return Time is
      begin
         return ada.calendar.arithmetic."+" (start_time, 5);
      end;

      start_date : QDateH := QDateTimeEdit_date (start_date_value);
      start_time : Time := time_of (QDate_year (start_date), QDate_month (start_date), QDate_day (start_date));

      end_date : QDateH := QDateTimeEdit_date (end_date_value);
      end_time : Time := time_of (QDate_year (end_date), QDate_month (end_date), QDate_day (end_date));

      maximum_start_time : time := get_maximum_start_time (end_time);
      maximum_start_date : QDateH := QDate_create (year (maximum_start_time), month (maximum_start_time), day (maximum_start_time));

      minimum_end_time : time := get_minimum_end_time (start_time);
      minimum_end_date : QDateH := QDate_create (year (minimum_end_time), month (minimum_end_time), day (minimum_end_time));
   begin
      QDateTimeEdit_setMaximumDate (start_date_value, maximum_start_date);
      QDateTimeEdit_setMinimumDate (end_date_value, minimum_end_date);
   end;

   procedure slot_compute_xph is
      c : country := country'val(QComboBox_currentIndex (country_choice));
      steps : integer := QSpinBox_value (steps_value);
      i,j,k,l,m : integer := steps + 1;
      size : integer := i*j*k*l*m;

      covid_data : country_entries_array := to_country_entries_array (country_entries);

      start_date : QDateH := QDateTimeEdit_date (start_date_value);
      start_time : Time := Ada.Calendar.Formatting.time_of (QDate_year (start_date), QDate_month (start_date), QDate_day (start_date));

      end_date : QDateH := QDateTimeEdit_date (end_date_value);
      end_time : Time := Ada.Calendar.Formatting.time_of (QDate_year (end_date), QDate_month (end_date), QDate_day (end_date));


      function get_date_index (date_time : time) return integer is
      begin
         for i in covid_data'range loop
            if covid_data (i).date = date_time then
               return i;
            end if;
         end loop;

         return -1;
      end;


      start_day_index : integer := get_date_index (start_time);

      end_day_index : integer := get_date_index (end_time);


      model : model_parameters;
      minimize_by_density_state : QtCheckState :=  QCheckBox_checkState (minimize_by_density);

      ua1 : uarray_access := new unknowns_array (1 .. size);
      ub1 : uarray_access := new unknowns_array (1 .. size);
      ub2 : uarray_access := new unknowns_array (1 .. size);
      uk1 : uarray_access := new unknowns_array (1 .. size);
      uk2 : uarray_access := new unknowns_array (1 .. size);

      ssrates : uarray_access := new unknowns_array (1 .. size);
      ssrates_by_density : uarray_access := new unknowns_array (1 .. size);

      forecast_value : integer := QSpinBox_value (forecast_days_value);
      forecast_ce : country_entries_array (1 .. forecast_value);
      refine_search : boolean := QWidget_isEnabled (QWidgetH (refine_search_group));
   begin

      --put_line (integer'image(start_day_index));
      --put_line (integer'image(end_day_index));
      QApplication_setOverrideCursor(QCursor_create(WaitCursor));

      reset_chart;

      build_search_set (steps, u_range, ua1, ub1, ub2, uk1, uk2);

      --Put_Line (integer'image(ua1'Length));

      compute_ssrate (c,
                      start_day_index,
                      end_day_index,
                      covid_data,
                      ua1,
                      ub1,
                      ub2,
                      uk1,
                      uk2,
                      ssrates,
                      ssrates_by_density);

      characterize_best_model (model,
                               ua1,
                               ub1,
                               ub2,
                               uk1,
                               uk2,
                               ssrates,
                               ssrates_by_density,
                               minimize_by_density_state = QtChecked);

      if refine_search then
         declare
            zoom_factor : float := float(QDoubleSpinBox_value (zoom_factor_value));
            minimal_improvement_percentage : float := float(QDoubleSpinBox_value (minimal_improvement_percentage_value));
         begin
            zoom (c,
                  steps,
                  start_day_index,
                  end_day_index,
                  covid_data,
                  minimize_by_density_state = QtChecked,
                  ua1,
                  ub1,
                  ub2,
                  uk1,
                  uk2,
                  ssrates,
                  ssrates_by_density,
                  model,
                  zoom_factor,
                  minimal_improvement_percentage);
         end;
      end if;

      compute_simulated_rate (c, start_day_index, covid_data, model);

      country_forecast_entries.clear;
      compute_forecast (c, covid_data, forecast_ce, model);
      country_forecast_entries := to_country_entries_vector (forecast_ce);

      update_chart;
      update_forecast_range_chart;

      QApplication_restoreOverrideCursor;
   end;


   procedure slot_change_country_choice(country_name : QStringH) is
      current_index : integer := QComboBox_currentIndex (country_choice);
      c : country := country'val(current_index);
      raw_ce : country_entries_array := get_country_data (data_filename, c);
      ce : country_entries_array := sanitize_covid_data (raw_ce, all_countries (c));


      function filter_out_toxic_date_time return country_entries_vector.vector is
         toxic_date : Time := Ada.Calendar.Formatting.time_of (2019, 12, 31);
      begin
         if ce (ce'first).date = toxic_date then
            return to_country_entries_vector (ce (ce'first + 1 .. ce'last));
         else
            return to_country_entries_vector (ce);
         end if;
      end;
   begin

      country_entries := filter_out_toxic_date_time;

      -- date
      set_initial_date_limits;
      --update_min_max_date_limits;

      -- chart
      reset_chart;

   end;

   procedure slot_start_date_changed (date: QDateH) is
   begin
      update_min_max_date_limits;
      update_chart;
      update_forecast_range_chart;
   end;

   procedure slot_end_date_changed (date: QDateH) is
   begin
      update_min_max_date_limits;
      update_chart;
      update_forecast_range_chart;
   end;

   procedure slot_change_forecast_days(foracast_days_val: Integer) is
   begin
      update_forecast_range_chart;
   end;

   function refine_search_group_enabled (state: integer) return boolean is
   begin
      if state = 0 then
         return false;
      end if;
      return true;
   end;

   procedure slot_change_refine_search (state: integer) is
   begin
      QWidget_setEnabled (QWidgetH (refine_search_group), refine_search_group_enabled (state));
   end;


end xph_model;
