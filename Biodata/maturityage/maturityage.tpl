// Simple 2-parameter model for estimating age at 50% maturity
// and standard deviation of age at 50% maturity
// logistic Maturity/Age relationship
DATA_SECTION
  init_int nobs
  init_vector age(1,nobs)
  init_vector mat(1,nobs)
  init_int eof
  !! eof==999 ? cout<<"(: --End of data file-- :)\n": cout<<"Error reading data file\n";

  vector a(1,nobs)

PARAMETER_SECTION
  init_bounded_number a50(2.0,10.0,1);
  init_bounded_number sigma_a50(0.00001,10.0,2);


  vector matpred(1,nobs);
  objective_function_value f;
  sdreport_number dummy;

PROCEDURE_SECTION
  matpred = 1 / (1 + exp(-(age - a50) / sigma_a50));
  f = norm2(matpred - mat);

REPORT_SECTION
  report<<"#a50 (Age at 50% maturity)"<<endl<<a50<<endl;
  report<<"#sigma_a50 (Standard deviation of age at 50% maturity)"<<endl<<sigma_a50<<endl;
