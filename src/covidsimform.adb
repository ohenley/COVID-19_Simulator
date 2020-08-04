with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions;  use Ada.Exceptions;

with Qt.QObject; use Qt.QObject;
with Qt.QUiLoader; use Qt.QUiLoader;
with Qt.QSpinBox; use Qt.QSpinBox;
with Qt.QWidget; use Qt.QWidget;
with Qt.QString; use Qt.QString;
with Qt.QLabel; use Qt.QLabel;
with Qt.QComboBox; use Qt.QComboBox;
with Qt.QStringList; use Qt.QStringList;
with Qt.QLineSeries; use Qt.QLineSeries;
with Qt.QXYSeries; use Qt.QXYSeries;
with Qt.QGraphicsView; use Qt.QGraphicsView;
with Qt.QChart; use Qt.QChart;
with Qt.QLegend; use Qt.QLegend;
with Qt.QChartView; use Qt.QChartView;
with Qt.QPainter; use Qt.QPainter;
with Qt.QLayout; use Qt.QLayout;
with Qt.QSize; use Qt.QSize;
with Qt.QGroupBox; use Qt.QGroupBox;
with Qt.QAbstractSeries; use Qt.QAbstractSeries;
with Qt.QValueAxis; use Qt.QValueAxis;
with Qt.QAbstractAxis; use Qt.QAbstractAxis;
with Qt.QObjectList; use Qt.QObjectList;
with Qt.QButton; use Qt.QButton;
with Qt.QStackedLayout; use Qt.QStackedLayout;
with Qt.QStackedWidget; use Qt.QStackedWidget;
with Qt.QDateTime; use Qt.QDateTime;
with Qt.QCheckBox; use Qt.QCheckBox;


with covid_19; use covid_19;


with xph_model; use xph_model;

package body CovidSimForm is


   simulation_engine_choice : QComboBoxH;
   simulation_engines : QStringListH  := QStringList_create;

   --stack : QStackedLayoutH;
   --stack_widget: QStackedWidgetH;



   scenario_choice : QComboBoxH;
   scenarios : QStringListH  := QStringList_create;

   graphic_view : QGraphicsViewH;
   chart  : QChartH;
   number_iterations : QSpinBoxH;
   number_population : QSpinBoxH;
   export_button : QAbstractButtonH;

   function enum_image_to_beautiful_image (image : String) return String is
      lower_name : String := to_lower (image);
   begin
      for i in lower_name'Range loop
         if lower_name(i) = '_' then
            lower_name(i) := ' ';
         end if;
      end loop;
      return lower_name;
   end;

   function beautiful_image_to_enum_image (lower_name : String) return String is
      upper_name : String := to_upper(lower_name);
   begin
      for i in upper_name'Range loop
         if upper_name(i) = ' ' then
            upper_name(i) := '_';
         end if;
      end loop;
      return upper_name;
   end;

   procedure init_simulation_engine_choice is
   begin
      simulation_engine_choice := QComboBoxH (QObject_findChild (QObjectH (covidsim_form), s2qs ("simulation_engine_choice")));

      for s in simulation_engine loop
         QStringList_append(handle => simulation_engines, s => s2qs(enum_image_to_beautiful_image(s'Image)));
      end loop;

      QComboBox_addItems (handle => simulation_engine_choice, texts  => simulation_engines);
   end;

   procedure update_line_chart (sim_data : Simulation_Data; scenario_beautiful_name: QStringH) is

      type Series is array (Status) of QSeriesH;
      data_series : Series;
      axes : QObjectListH;

   begin

      QChart_removeAllSeries (chart);

      for i in sim_data'range (1) loop

         data_series(i) := QLineSeries_create;
         QAbstractSeries_setName(data_series(i), s2qs(enum_image_to_beautiful_image(i'Image)));

         for j in sim_data'range (2) loop
            QXYSeries_append (data_series (i), qreal (j), qreal (sim_data(i,j)));
         end loop;

         QChart_addSeries (chart, data_series(i));

      end loop;

      QChart_createDefaultAxes (chart);

      axes := QChart_axes(chart);

      QAbstractAxis_setTitletext(QAxisH(QObjectList_at(axes, 0)), s2qs("Iterations"));
      QValueAxis_setLabelFormat(QAxisH(QObjectList_at(axes, 0)), s2qs("%d"));

      QAbstractAxis_setTitletext(QAxisH(QObjectList_at(axes, 1)), s2qs("Population"));
      QValueAxis_setLabelFormat(QAxisH(QObjectList_at(axes, 1)), s2qs("%d"));

      QChart_setTitle (chart, scenario_beautiful_name);

   exception
      when Error: others =>
         Put ("Unexpected exception: ");
         Put_Line (Exception_Information(Error));
   end;

   procedure update_simulation is
      scenario_name : String := beautiful_image_to_enum_image(qs2s(QComboBox_currentText(scenario_choice)));
      results : Simulation_Data := Simulation(Scenario'Value(scenario_name), QSpinBox_value(number_population), QSpinBox_value(number_iterations));
   begin
      update_line_chart(results, QComboBox_currentText(scenario_choice));
   end;

   procedure set_simulation_engine_panel is
      simulation_engine_name : String := beautiful_image_to_enum_image(qs2s(QComboBox_currentText(simulation_engine_choice)));
      simulation_choice : Simulation_Engine := Simulation_Engine'Value(simulation_engine_name);
      stack : QStackedWidgetH := QStackedWidgetH (QObject_findChild (QObjectH (covidsim_form), s2qs ("simulation_panels")));
   begin

      if simulation_choice = XPH_Pharmaceutical then
         QStackedWidget_setCurrentIndex (stack, 0);
         slot_change_country_choice (s2qs (""));
      end if;

      if simulation_choice = Lancet then
         QStackedWidget_setCurrentIndex (stack, 1);
         update_simulation; -- lancet model stuff TODO : move to lancet_model
      end if;
   end;

   procedure init_chart is
      legend : QLegendH;
      chart_view : QGraphicsViewH;
      horizontal_layout : QBoxLayoutH := QHBoxLayout_create;
   begin
      chart  := QChart_create;

      legend := QChart_legend (chart);
      QLegend_setVisible (legend, true);

      chart_view := QChartView_create (chart);
      QGraphicsView_setRenderHint (chart_view, QPainterAntialiasing);
      QBoxLayout_addWidget (horizontal_layout, QWidgetH(chart_view));

      QWidget_setLayout (QwidgetH (graphic_view), horizontal_layout);
   end;


   procedure init_scenario_choices is
   begin
      scenario_choice := QComboBoxH (QObject_findChild (QObjectH (covidsim_form), s2qs ("scenario_choice")));

      for s in Scenario loop
         QStringList_append(handle => scenarios, s => s2qs(enum_image_to_beautiful_image(s'Image)));
      end loop;

      QComboBox_addItems (handle => scenario_choice, texts  => scenarios);
   end;

   procedure set_xph_model_ui (form : QWidgetH) is
      compute_xph_button : QPushButtonH := QPushButtonH (QObject_findChild (QObjectH (covidsim_form), s2qs ("compute_xph")));
      country_choice : QComboBoxH := QComboBoxH (QObject_findChild (QObjectH (covidsim_form), s2qs ("country_choice")));
      start_date_value : QDateEditH := QDateEditH (QObject_findChild (QObjectH (covidsim_form), s2qs ("start_date_value")));
      end_date_value : QDateEditH := QDateEditH (QObject_findChild (QObjectH (covidsim_form), s2qs ("end_date_value")));
      forecast_days_value : QSpinBoxH := QSpinBoxH (QObject_findChild (QObjectH (covidsim_form), s2qs ("forecast_days_value")));
      refine_search : QCheckBoxH := QCheckBoxH (QObject_findChild (QObjectH (covidsim_form), s2qs ("refine_search")));
   begin

      QAbstractButton_signal_slot_clicked (compute_xph_button, slot_compute_xph'access);
      QComboBox_signal_slot_activated2 (country_choice, slot_change_country_choice'access);
      QDateEdit_signal_slot_userDateChanged (start_date_value, slot_start_date_changed'access);
      QDateEdit_signal_slot_userDateChanged (end_date_value, slot_end_date_changed'access);
      QSpinBox_signal_slot_valueChanged (forecast_days_value, slot_change_forecast_days'access);
      QCheckBox_signal_slot_stateChanged (refine_search, slot_change_refine_search'access);

      init_model (form, chart);
   end;



   procedure covidsim_form_init (parent : QWidgetH := null) is
   begin
      -- create the UI based on QTDesigner .ui file specification
      covidsim_form := QUiLoader_loadFromFile (QUiLoader_create, s2qs (Current_Directory & "../../../../../../src/form/covidsim_form.ui"));

      -- fetch and 'cache' the widgets we want to manipulate, by name, from our .ui design
      graphic_view := QGraphicsViewH (QObject_findChild (QObjectH (covidsim_form), s2qs ("graphic_view")));
      number_iterations := QSpinBoxH (QObject_findChild (QObjectH (covidsim_form), s2qs ("number_iterations")));
      number_population := QSpinBoxH (QObject_findChild (QObjectH (covidsim_form), s2qs ("number_population")));
      export_button := QAbstractButtonH (QObject_findChild (QObjectH (covidsim_form), s2qs ("export_to_csv")));

      -- populate 'complex' widgets
      init_chart;
      init_scenario_choices;

      init_simulation_engine_choice;

      set_simulation_engine_panel;

      -- define and set qt 'callbacks' on widgets of interest
      QComboBox_signal_slot_activated2 (simulation_engine_choice, slot_change_simulation_engine'access);
      QComboBox_signal_slot_activated2 (scenario_choice, slot_change_scenario'access);
      QSpinBox_signal_slot_valueChanged (number_iterations, slot_change_iterations'access);
      QSpinBox_signal_slot_valueChanged (number_population, slot_change_population'access);
      QAbstractButton_signal_slot_clicked (export_button, slot_export_to_csv'access);

      set_xph_model_ui (covidsim_form);

      -- we do a first draw of the chart
      update_simulation;

   exception
      when Error: others =>
         Put ("Unexpected exception: ");
         Put_Line (Exception_Information(Error));
   end;

   procedure slot_change_simulation_engine (simulation_engine_beautiful_name: QStringH) is
   begin
      set_simulation_engine_panel;
   end;

   procedure slot_change_scenario (scenario_beautiful_name: QStringH) is
   begin
      update_simulation;
   end;

   procedure slot_change_iterations (iterations: Integer) is
   begin
      update_simulation;
   end;

   procedure slot_change_population (population: Integer) is
   begin
      update_simulation;
   end;

   procedure slot_export_to_csv is
      scenario_name : String := beautiful_image_to_enum_image(qs2s(QComboBox_currentText(scenario_choice)));
      results : Simulation_Data := Simulation(Scenario'Value(scenario_name), QSpinBox_value(number_population), QSpinBox_value(number_iterations));
   begin
      Export_To_CSV (results, Scenario'Value(scenario_name), QSpinBox_value(number_population), QSpinBox_value(number_iterations));
   end;






end;
