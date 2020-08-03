with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;

with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;
with Ada.Calendar; use Ada.Calendar;

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

with Qt.QScatterSeries;      use Qt.QScatterSeries;


with Qt.QPen;            use Qt.QPen;
with Qt.QBrush;          use Qt.QBrush;
with Qt.QPainter;        use Qt.QPainter;
with Qt.QGradient;       use Qt.QGradient;
with Qt.QAbstractSeries; use Qt.QAbstractSeries;

-- TODO:
-- 1. 12/31/2019 is a problematic date. (does not want to move at any day, month or year because its is always more difference than what is legit -> end_day-start_day) start one day after. Sanitize data
-- 2. in slot_compute_xph, find start_day_index from start_date and end_day_index from end_date
-- 3. debug smallest ssrates ... they are way too high
-- 4. generalize first pass and zoom
-- 5. implement progress bar, feedback and cancel.


package body xph_model is

   data_filename : string := "../../../../../deps/xph_covid19/data/covid19.csv";

   --c : country;
   start_day_index : integer := 68; -- start day, from the beginning
   end_day_index : integer := 163; -- end day for special cases
   steps : integer := 20;
   --minimize_by_density : boolean := false;
   zoom_factor : float := 4.0;
   minimal_improvement_percentage : float := 0.3;
   fore_term : integer := 400; -- days total (data+forecast)
   bend_percent : float := 0.85;



   bend : integer := 1; -- array index for bend in forecast_ce
   first_case : integer := 6; -- arr. index in forecast_ce when 1'st case appeared


   country_entries : country_entries_vector.vector;

   chart : QChartH;
   form : QWidgetH;

   country_choice : QComboBoxH;
   start_date_value : QDateEditH;
   end_date_value : QDateEditH;
   steps_value : QSpinBoxH;
   forecast_days_value : QSpinBoxH;
   minimize_by_density : QCheckBoxH;

   --timer : QTimerH;

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


   procedure update_ground_truth_chart is
      current_index : integer := QComboBox_currentIndex (country_choice);
      c : country := country'val(current_index);

      start_date : QDateH := QDateTimeEdit_date (start_date_value);
      start_time : Time := time_of (QDate_year (start_date), QDate_month (start_date), QDate_day (start_date));

      end_date : QDateH := QDateTimeEdit_date (end_date_value);
      end_time : Time := time_of (QDate_year (end_date), QDate_month (end_date), QDate_day (end_date));

      country_ground_truth_first : QSeriesH;
      country_ground_truth_date_range : QSeriesH;
      country_ground_truth_last : QSeriesH;

      procedure init_series is
         pen_first : QPenH := QPen_create(QColor_create(0,255,255));
         pen_date_range : QPenH := QPen_create(QColor_create(255,0,0));
         pen_last : QPenH := QPen_create(QColor_create(0,255,255));
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
      end;

   begin
      QChart_removeAllSeries (chart);

      QChart_setTitle (chart, s2qs (all_countries(c).name));

      init_series;

      for e of country_entries loop

         if ada.calendar.arithmetic."-" (start_time, e.date) > 0 then
            QXYSeries_append (country_ground_truth_first, qreal(e.day_index), qreal(e.cumulative_cases));
         elsif ada.calendar.arithmetic."-" (e.date, end_time) > 0 then
            QXYSeries_append (country_ground_truth_last, qreal(e.day_index), qreal(e.cumulative_cases));
         else
            QXYSeries_append (country_ground_truth_date_range, qreal(e.day_index), qreal(e.cumulative_cases));
         end if;

      end loop;

      QChart_addSeries (chart, country_ground_truth_first);
      QChart_addSeries (chart, country_ground_truth_date_range);
      QChart_addSeries (chart, country_ground_truth_last);

      QChart_createDefaultAxes (chart);

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

      init_country_choices;

   end;

   procedure set_initial_date_limits is
      start_time : Time := country_entries.first_element.date;
      start_date : QDateH := QDate_create (year (start_time), month (start_time), day (start_time));
      end_time : Time := country_entries.last_element.date;
      end_date : QDateH := QDate_create (year (end_time), month (end_time), day (end_time));
   begin
      QDateTimeEdit_setMinimumDate (start_date_value, start_date);
      QDateTimeEdit_setDate (start_date_value, start_date);

      QDateTimeEdit_setDate (end_date_value, end_date);
      QDateTimeEdit_setMaximumDate (end_date_value, end_date);
   end;

   function get_maximum_start_time(end_time : time) return Time is
   begin
      return ada.calendar.arithmetic."-" (end_time, 5);
   end;

   function get_minimum_end_time (start_time : time) return Time is
   begin
      return ada.calendar.arithmetic."+" (start_time, 5);
   end;

   procedure update_min_max_date_limits is
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
      i,j,k,l,m : integer := QSpinBox_value (steps_value) + 1;
      size : integer := i*j*k*l*m;
      covid_data : country_entries_array := to_country_entries_array (country_entries);
      model : model_parameters;
      minimize_by_density_state : QtCheckState :=  QCheckBox_checkState (minimize_by_density);
      ua1 : uarray_access := new unknowns_array (1 .. size);
      ub1 : uarray_access := new unknowns_array (1 .. size);
      ub2 : uarray_access := new unknowns_array (1 .. size);
      uk1 : uarray_access := new unknowns_array (1 .. size);
      uk2 : uarray_access := new unknowns_array (1 .. size);

      ssrates : uarray_access := new unknowns_array (1 .. size);
      ssrates_by_density : uarray_access := new unknowns_array (1 .. size);
   begin

      build_search_set (steps, u_range, ua1, ub1, ub2, uk1, uk2);

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
      show_model_unknows (model);


   end;


   procedure slot_change_country_choice(country_name : QStringH) is
      current_index : integer := QComboBox_currentIndex (country_choice);
      c : country := country'val(current_index);
      raw_ce : country_entries_array := get_country_data (data_filename, c);
      ce : country_entries_array := sanitize_covid_data (raw_ce, all_countries (c));
   begin

      country_entries := to_country_entries_vector (ce);

      -- date
      set_initial_date_limits;
      update_min_max_date_limits;

      -- chart
      --init_chart;
      update_ground_truth_chart;
      update_forecast_range_chart;

   end;

   procedure slot_start_date_changed (date: QDateH) is
   begin
      --init_chart;
      update_min_max_date_limits;
      update_ground_truth_chart;
      update_forecast_range_chart;
   end;

   procedure slot_end_date_changed (date: QDateH) is
   begin
      --init_chart;
      update_min_max_date_limits;
      update_ground_truth_chart;
      update_forecast_range_chart;
   end;

   procedure slot_change_forecast_days(foracast_days_val: Integer) is
   begin
      update_forecast_range_chart;
   end;

end xph_model;



   --  procedure slot_compute_xph is
   --     long_p : long_process;
   --  begin
   --     timer := QTimer_create;
   --
   --     QTimer_signal_slot_timeout(handle => timer, hook => update'access);
   --     QTimer_start (timer, 1000);
   --     long_p.start;
   --  end;

   --  procedure slot_compute_xph is
   --     c : country := country'val(QComboBox_currentIndex (country_choice));
   --     i,j,k,l,m : integer := QSpinBox_value (steps_value) + 1;
   --     size : integer := i*j*k*l*m;
   --     covid_data : country_entries_array := to_country_entries_array (country_entries);
   --     model : model_parameters;
   --     minimize_by_density_state : QtCheckState :=  QCheckBox_checkState (minimize_by_density_value);
   --     minimize_by_density : boolean;
   --  begin
   --
   --     ua1 := new unknowns_array (1 .. size);
   --     ub1 := new unknowns_array (1 .. size);
   --     ub2 := new unknowns_array (1 .. size);
   --     uk1 := new unknowns_array (1 .. size);
   --     uk2 := new unknowns_array (1 .. size);
   --
   --     ssrates := new unknowns_array (1 .. size);
   --     ssrates_by_density := new unknowns_array (1 .. size);
   --
   --     build_search_set (steps, u_range, ua1, ub1, ub2, uk1, uk2);
   --
   --     declare
   --        result : boolean := compute_ssrate (c,
   --                                            start_day_index,
   --                                            end_day_index,
   --                                            covid_data,
   --                                            ua1,
   --                                            ub1,
   --                                            ub2,
   --                                            uk1,
   --                                            uk2,
   --                                            ssrates,
   --                                            ssrates_by_density);
   --     begin
   --        Put_Line ("calisss!");
   --     end;
   --
   --     minimize_by_density := minimize_by_density_state = QtChecked;
   --     characterize_best_model (model,
   --                              ua1,
   --                              ub1,
   --                              ub2,
   --                              uk1,
   --                              uk2,
   --                              ssrates,
   --                              ssrates_by_density,
   --                              minimize_by_density);
   --     show_model_unknows (model);
   --  end;



   --  procedure update is
   --        model : model_parameters;
   --        minimize_by_density_state : QtCheckState :=  QCheckBox_checkState (minimize_by_density_value);
   --        minimize_by_density : boolean;
   --     begin
   --     if control.are_ready then
   --        control.set_ready (false);
   --           minimize_by_density := minimize_by_density_state = QtChecked;
   --           characterize_best_model (model,
   --                                    ua1,
   --                                    ub1,
   --                                    ub2,
   --                                    uk1,
   --                                    uk2,
   --                                    ssrates,
   --                                    ssrates_by_density,
   --                                    minimize_by_density);
   --           show_model_unknows (model);
   --        end if;
   --     end;
   --
   --  task type long_process is
   --     entry start;
   --  end;
   --
   --  task body long_process is
   --     c : country := country'val(QComboBox_currentIndex (country_choice));
   --     i,j,k,l,m : integer := QSpinBox_value (steps_value) + 1;
   --     size : integer := i*j*k*l*m;
   --     covid_data : country_entries_array := to_country_entries_array (country_entries);
   --  begin
   --
   --     ua1 := new unknowns_array (1 .. size);
   --     ub1 := new unknowns_array (1 .. size);
   --     ub2 := new unknowns_array (1 .. size);
   --     uk1 := new unknowns_array (1 .. size);
   --     uk2 := new unknowns_array (1 .. size);
   --
   --     ssrates := new unknowns_array (1 .. size);
   --     ssrates_by_density := new unknowns_array (1 .. size);
   --
   --     control.set_ready (false);
   --     accept start;
   --     build_search_set (steps, u_range, ua1, ub1, ub2, uk1, uk2);
   --     --  compute_ssrate (c,
   --     --                  start_day_index,
   --     --                  end_day_index,
   --     --                  covid_data,
   --     --                  ua1,
   --     --                  ub1,
   --     --                  ub2,
   --     --                  uk1,
   --     --                  uk2,
   --     --                  ssrates,
   --     --                  ssrates_by_density);
   --     control.set_ready (true);
   --  end;


   --  protected control is
   --     procedure set_ready (ready : boolean);
   --     function are_ready return boolean;
   --
   --     procedure set_abort (stop : boolean);
   --     function get_abort return boolean;
   --  private
   --     results_ready : boolean := false;
   --     asked_abort : boolean := false;
   --  end;
   --
   --  protected body control is
   --     procedure set_ready (ready : boolean) is
   --     begin
   --        results_ready := ready;
   --     end;
   --
   --     function are_ready return boolean is
   --     begin
   --        return results_ready;
   --     end;
   --
   --     procedure set_abort (stop : boolean) is
   --     begin
   --        asked_abort := stop;
   --     end;
   --
   --     function get_abort return boolean is
   --     begin
   --        return asked_abort;
   --     end;
   --  end;
