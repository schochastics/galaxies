#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double IntensityBulge(double R, double I0, double k)
{
  return I0 * exp(-k*pow(R, 0.25));
}

//-------------------------------------------------------------------------------------------------
// [[Rcpp::export]]
double IntensityDisc(double R, double I0, double a)
{
  return I0 * exp(-R/a);
}

//-------------------------------------------------------------------------------------------------
// [[Rcpp::export]]
double Intensity(double x,double m_RBulge,double m_I0, double m_k, double m_a)
{
  return (x<m_RBulge) ? IntensityBulge(x, m_I0, m_k) : IntensityDisc(x-m_RBulge, IntensityBulge(m_RBulge, m_I0, m_k), m_a);
}

// [[Rcpp::export]]
List BuildCDF(int nSteps,double m_fMax,double m_fMin,
              double m_RBulge,double m_I0, double m_k, double m_a)
{
  double h = (m_fMax - m_fMin) / nSteps;
  double x=0, y=0;
  
  std::vector<double> m_vX1;
  std::vector<double> m_vY1;
  std::vector<double> m_vX2;
  std::vector<double> m_vY2;
  std::vector<double> m_vM1;
  std::vector<double> m_vM2;
  
  // Simpson rule for integration of the distribution function
  m_vY1.push_back(0.0);
  m_vX1.push_back(0.0);
  for (int i=0; i<nSteps; i+=2)
  {
    x = (i+2) *h;
    y += h/3 * (Intensity(m_fMin + i*h,m_RBulge,m_I0,m_k,m_a) + 4*Intensity(m_fMin + (i+1)*h,m_RBulge,m_I0,m_k,m_a) + Intensity(m_fMin + (i+2)*h,m_RBulge,m_I0,m_k,m_a));
    
    m_vM1.push_back((y - m_vY1.back()) / (2*h));
    m_vX1.push_back(x);
    m_vY1.push_back(y);
    
  }
  m_vM1.push_back(0.0);
  
  // all arrays must have the same length
  if (m_vM1.size()!=m_vX1.size() || m_vM1.size()!=m_vY1.size())
    throw std::runtime_error("CumulativeDistributionFunction::BuildCDF: array size mismatch (1)!");
  
  for (std::size_t i=0; i<m_vY1.size(); ++i)
  {
    m_vY1[i] /= m_vY1.back();
    m_vM1[i] /= m_vY1.back();
  }
  
  m_vX2.push_back(0.0);
  m_vY2.push_back(0.0);
  
  double p=0;
  h = 1.0/nSteps;
  for (int i=1, k=0; i<nSteps; ++i)
  {
    p = (double)i * h;
    
    for (; m_vY1[k+1]<=p; ++k)
    {}
    
    
    y = m_vX1[k] + (p - m_vY1[k]) / m_vM1[k];
    
    m_vM2.push_back( (y - m_vY2.back()) / h);
    m_vX2.push_back(p);
    m_vY2.push_back(y);
  }
  m_vM2.push_back(0.0);
  
  // all arrays must have the same length
  if (m_vM2.size()!=m_vX2.size() || m_vM2.size()!=m_vY2.size())
    throw std::runtime_error("BuildCDF: array size mismatch (1)!");
  
  return(List::create(Rcpp::Named("m_vX1")  = m_vX1,
                      Rcpp::Named("m_vY1")  = m_vY1,
                      Rcpp::Named("m_vX2")  = m_vX2,
                      Rcpp::Named("m_vY2")  = m_vY2,
                      Rcpp::Named("m_vM1")  = m_vM1,
                      Rcpp::Named("m_vM2")  = m_vM2));
}