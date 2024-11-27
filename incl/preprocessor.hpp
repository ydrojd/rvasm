#ifndef PREPROCESOR_HPP
#define PREPROCESOR_HPP

#include "statement.hpp"
#include "error_logger.hpp"
#include <vector>

std::vector<statement> preprocess(std::vector<statement> &&input_stmnts, std::vector<syntax_error> &errors);

#endif
