#include <Rcpp.h>
using namespace Rcpp;

#include <sstream> // for to_string
// std::to_string doesn't work for some reason, will have to workaround
template < typename T > std::string to_string( const T& n )
{
  std::ostringstream stm ;
  stm << n ;
  return stm.str() ;
}

//' Helps model.longNote in speeding up operations
//'
//' @param chart A casted chart provided via model.longNote
//' @param longNoteStartName The string indicating the start of a long note
//' @param longNoteEndName The string indicating the start of a long note
//' @param newName The string indicating the replaced name of long note bodies
//' @param labelStart A boolean indicating if the start of the long notes must
//' be labelled
//' @param labelEnd A boolean indicating if the end of the long notes must be
//' labelled
//'
//' @export
//'
// [[Rcpp::export(name=".cppModelLongNote")]]
List cppModelLongNote(DataFrame chart,
                      std::string longNoteStartName,
                      std::string longNoteEndName,
                      std::string newName,
                      bool labelStart,
                      bool labelEnd) {

  unsigned int cols = chart.ncol();
  unsigned int rows = chart.nrow();
  bool lnFlag = false;

  List out = List::create(_["offsets"] = chart["offsets"]);

  for (unsigned int col = 1; col < cols; col++) {
    CharacterVector colVector = chart[col];
    lnFlag = false;
    for (unsigned int row = 1; row < rows; row++) {

      // If Start is encountered
      if (colVector[row] == longNoteStartName) {
        lnFlag = true;
        if (labelStart) colVector[row] = newName;

      // If End is encountered
      } else if (colVector[row] == longNoteEndName) {
        lnFlag = false;
        if (labelEnd) colVector[row] = newName;

      // If default is encountered
      } else {
        if (lnFlag) colVector[row] = newName;
      }
    }
    out.push_back(colVector, to_string(col));
  }
  return out;
}

