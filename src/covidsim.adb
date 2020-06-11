with Qt; use Qt;
with Qt.QApplication; use Qt.QApplication;
with Qt.QWidget; use Qt.QWidget;
with CovidSimForm; use CovidSimForm;

procedure covidsim is

begin

  covidsim_form_init;
  QWidget_show(covidsim_form);
  QApplication_invoke;

end;
