// Simple 2-parameter model for estimating B0 and B1 in a
// logistic Maturity/Age relationship
DATA_SECTION
  init_int nobs
  init_vector age(1,nobs)
  init_vector mat(1,nobs)
  init_int eof
  !! eof==999 ? cout<<"(: --End of data file-- :)\n": cout<<"Error reading data file\n";

  vector a(1,nobs)

PARAMETER_SECTION
  init_bounded_number K(0.00001,10.0);
  init_bounded_number C(0.00001,4.0);

  vector matpred(1,nobs);
  objective_function_value f;
  sdreport_number dummy;

PROCEDURE_SECTION
  matpred = 1 / (1 + exp(- K * (age - C)));
  f = norm2(matpred - mat);

REPORT_SECTION
  report<<"#C (Age at 50%)"<<endl<<C<<endl;
  report<<"#K (Instantaneous rate of fish maturation)"<<endl<<K<<endl;
