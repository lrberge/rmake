#include <Rcpp.h>
#include <math.h>
#include <vector>
#include <stdio.h>
#include <cmath>
#include <functional>
#include <string>
#include <filesystem>
#include <fstream>

using namespace Rcpp;

namespace fs = std::filesystem;

char table_digit_letter[37] = "0123456789abcdefghijklmnopqrstuvwxyz";

// [[Rcpp::export]]
std::string cpp_hash_string(std::string x){
  // Algorithm from: http://www.cse.yorku.ca/~oz/hash.html
  
  int n = x.size();
  
  if(n == 0){
    return "0";
  }
  
  unsigned long long hash = 5381;
  
  for(int i=0 ; i<n ; ++i){
    hash = ((hash << 5) + hash) + x[i]; /* hash * 33 + c */
  }
  
  std::string res;
  unsigned long long z = hash;
  unsigned long long r = 0;
  
  while(z > 36){
    r = z % 36;
    z = (z - r) / 36;
    res += table_digit_letter[r];
  }
  
  return res;
}

inline bool is_name_char(const char &z){
  return (z >= 'a' && z <= 'z') || (z >= 'A' && z <= 'Z') || (z >= '0' && z <= '9') || z == '.' || z == '_';
}


int find_fun_substr(const char * x, const char * pattern, int start = 0){
  
  int n = std::strlen(x);
  int n_pat = std::strlen(pattern);
  
  // +2, to add the paren: pattern()
  if(n < n_pat + 2) return -1;
  
  int i = start;
  while(i < n && i + n_pat + 1 < n){
    
    if(x[i] == pattern[0] && (i == 0 || !is_name_char(x[i - 1]))){
      
      int j = 1;
      while(j < n_pat && x[i + j] == pattern[j]){
        ++j;
      }
      
      if(j == n_pat && x[i + j] == '('){
        // match!
        return i;
      }
      
      i += j;
    } else {
      ++i;
    }
  }
  
  return -1;
}

// [[Rcpp::export]]
std::vector<std::string> cpp_extract_fun(SEXP R_str, SEXP R_fun_str){
  // R_str must be of length 1
  
  const char * x = CHAR(STRING_ELT(R_str, 0));
  const char * fun = CHAR(STRING_ELT(R_fun_str, 0));
  
  int n = std::strlen(x);
  
  int start = 0;
  int i = 0;
  
  bool ok = false;
  
  std::vector<std::string> res_all;
  
  while(i < n){
    int pos = find_fun_substr(x, fun, start);
    
    if(pos == -1) break;
    
    std::string res;
    i = pos;
    while(x[i] != '('){
      res += x[i++];
    }
    
    ok = false;
    int n_paren = 0;
    
    while(i < n){
      if(x[i] == '('){
        n_paren++;
        res += '(';
      } else if(x[i] == ')'){
        n_paren--;
        res += ')';
        if(n_paren == 0){
          ok = true;
          start = i++;
          res_all.push_back(res);
          break;
        }
      } else if(x[i] == '"' || x[i] == '\''){
        char quote = x[i];
        res += quote;
        ++i;
        while(i < n && x[i] != quote){
          res += x[i++];
        }
        
        if(x[i] != quote){
          // problem
          break;
        }
        
        res += quote;
      } else {
        res += x[i];
      }
      
      ++i;
    }
    
    if(!ok){
      Rcout << "Extraction of the function did not work. Current collection:\n  " << res << "\n";
      return res_all;
    }
  }
  
  return res_all;
}

inline bool is_quote(const char x){
  return x == '\'' || x == '"';
}

inline bool is_space(const char x){
  return x == ' ' || x == '\t' || x == '\r';
}

inline bool is_not_space(const char x){
  return !(x == ' ' || x == '\t' || x == '\r');
}

inline bool is_space_nl(const char x){
  return x == ' ' || x == '\t' || x == '\r' || x == '\n';
}

// [[Rcpp::export]]
std::string cpp_clean_comments(SEXP R_str){
  
  const char * x = CHAR(STRING_ELT(R_str, 0));
  int n = std::strlen(x);
  
  std::string res;
  char quote = '\'';
  
  int i = 0;
  while(i < n){
    // first we go to the next non empty element of the next non empty line
    while(i < n && is_space_nl(x[i])) ++i;
    
    if(i == n) break;
    
    if(x[i] == '/' && i + 1 < n){
      if(x[i + 1] == '/'){
        // single line comment
        // => we go to the end of the comment (we also take care of continuing comments with backslash)
        i += 2;
        while(i < n && (x[i] != '\n' || x[i - 1] == '\\')) ++i;
        continue;
      } else if(x[i + 1] == '*'){
        /*
         * Multiple line comments 
         */
        while(i < n && (x[i] != '/' || x[i - 1] == '*')) ++i;
        // we go to the next line
        while(i < n && x[i] != '\n') ++i;
        continue;
      }
    }
    
    // we go through the line  
    
    bool is_comment = false;
    while(i < n && x[i] != '\n'){
      if(x[i] == '\'' || x[i] == '"'){
        quote = x[i++];
        res += quote;
        while(i < n && (x[i] != quote || (x[i - 1] == '\\' && (i > 2 && x[i - 2] != '\\')))){
          res += x[i++];
        }
        res += quote;
        ++i;
      } else {
        if(x[i] == '/' && i + 1 < n && (x[i + 1] == '/' || x[i + 1] == '*')){
          // this is a comment => we get out, back at the initial 'while'
          is_comment = true;
          break;
        }
        
        if(is_space(x[i])){
          // we normalize the spaces
          ++i;
          while(i < n && is_space(x[i])) ++i;
          if(i < n && x[i] != '\n') res += ' ';
        } else {
          res += x[i++];
        }
      }
    }
    
    if(!is_comment){
      ++i;
      res += '\n'; 
    }
  }
  
  return res;
}

inline bool is_operator(const char x){
  return x == '+' || x == '-' || x == '*' || x == '/' || x == '%';
}

// [[Rcpp::export]]
Rcpp::List cpp_valid_code_index(SEXP R_str){
  // R_str: code in the form of text
  // OUT: reports the lines which contains a coding statement that will be executed
  // does not report codelines which are the completeion of previous code lines
  // example:
  // 7| data %>%
  // 8|   print()
  // => here only the 7th line will be reported as valid code
  // Remember that we will insert "LINE_COUNT = nb" just before each line!!!!
  // so that can't be done for every line.
  //
  // Same for lines equal to "}".
  // example:
  // 2| test = function(){
  // 3|    5 + 5
  // 4| }
  // => here you want 3 and 4, but no the 5th line! Because otherwise LINE_COUNT will be the return statement.

  int n_lines = Rf_length(R_str);
  Rcpp::IntegerVector line_nb;
  Rcpp::IntegerVector size;
  
  int i_origin = 1;

  // a few variables
  char quote = ' ';
  int n_open_par = 0;
  int n_open_sqBracket = 0;

  int i = 0;

  while (i < n_lines) {
    const char * line = CHAR(STRING_ELT(R_str, i));
    int n_chars = std::strlen(line);
    int j = 0;

    // we skip the blanks and go straight to the first char
    for(; j < n_chars && is_space(line[j]) ; ++j);

    if (j == n_chars || line[j] == '#' || line[j] == '}') {
      ++i;
      continue;
    }

    // This is a valid line
    i_origin = i;
    line_nb.push_back(i + 1);

    // now let's see how far that line continues
    n_open_par = 0;
    n_open_sqBracket = 0;
    
    bool init = true;
    char last_char = '_';
    while (i < n_lines) {
      if (init) {
        init = false;
      } else {
        line = CHAR(STRING_ELT(R_str, i));
        n_chars = std::strlen(line);
        j = 0;
        for(; j < n_chars && is_space(line[j]) ; ++j);
        if(j == n_chars || line[j] == '#'){
          ++i;
          continue;
        }
      }

      while (j < n_chars) {
        const char val = line[j];
        
        if(is_quote(val)){
          quote = val;
          ++j;
          // we go to the end of the quote
          // going through as many lines as needed
          while (i < n_lines) {
            // we go to the next quote in the line, if present
            for(; j < n_chars && (line[j] != quote || (j > 0 && line[j - 1] == '\\')) ; ++j);
            
            if(j == n_chars){
              // we're still in a quote
              ++i;
              if(i < n_lines){
                line = CHAR(STRING_ELT(R_str, i));
                n_chars = std::strlen(line);
                j = 0;
              }
            } else {
              // we are at the end of the quote
              break;
            }
          }
        } else if(val == '#'){
          // we go to the next line
          ++i;
          break;
        } else if(val == '['){
          ++n_open_sqBracket;
        } else if(val == ']'){
          --n_open_sqBracket;
        } else if(val == '('){
          ++n_open_par;
        } else if(val == ')'){
          --n_open_par;
        }

        // we keep the last character in memory
        if(is_not_space(val)){
          last_char = val;
        }

        ++j;
      }

      // shall we stop?
      // we stop when:
      // - all open par/brackets are closed
      // - there is no operator waiting

      ++i;
      if(n_open_par != 0 || n_open_sqBracket != 0 || is_operator(last_char)){
        // we continue
      } else {
        // we quit and can go fetch the next valid line
        size.push_back(i - i_origin);
        break;
      }
    }
  }
  
  List res;
  res["line_nb"] = line_nb;
  res["size"] = size;

  return res;
}


void absolute_to_relative_single(const char *path, const char *root, std::string &res){
  // NOTA: root NEVER contains a trailing slash
  
  res.clear();
  
  int n_path = std::strlen(path);
  int n_root = std::strlen(root);
  
  // we ignore the root's trailing slash if provided
  if(root[n_root - 1] == '/') --n_root;
  
  int n = n_path < n_root ? n_path : n_root;
  
  int i = 0;
  int i_max_same = 0;
  int n_same_segments = 0;
  while(i < n && path[i] == root[i]){
    if(path[i] == '/'){
      ++n_same_segments;
      i_max_same = i;
    }
    ++i;
  }
  
  // p: bon/jour/les/gens
  // r: bon/jour
  // vs
  // p: bon/journee/les/gens
  // r: bon/jour
  if(i == n){
    if(n_path > n && path[n] == '/'){
      // p: bon/jour/les/gens
      // r: bon/jour
      for(int j = n + 1 ; j < n_path ; ++j){
        res += path[j];
      }
      return;
    } else if(n_path == n_root){
      // p: bon/jour
      // r: bon/jour
      res = ".";
      return;
    }
  }
  
  // p: C:/bon/jour
  // r: D:/hello/guys
  if(n_same_segments == 0){
    // different drives
    for(int j = 0 ; j < n_path ; ++j){
      res += path[j];
    }
    return;
  }
  
  // p: bon/jour/les/gens
  // r: bon/jour/mes/dames
  int n_going_back = 1;
  for(int j = i_max_same + 1 ; j < n_root ; ++j){
    if(root[j] == '/'){
      // remember that the root folder never contains a trailing slash
      // that's why I init n_going_back to 1
      ++n_going_back;
    }
  }
  
  for(int j = 0 ; j < n_going_back ; ++j){
    res += "../";
  }
  
  for(int j = i_max_same + 1 ; j < n_path ; ++j){
    res += path[j];
  }
  
}

// [[Rcpp::export]]
SEXP cpp_absolute_to_relative(SEXP R_paths, SEXP R_root){
  
  int n = Rf_length(R_paths);
  
  const char *root_path = Rf_translateCharUTF8(STRING_ELT(R_root, 0));
  
  SEXP res_all = PROTECT(Rf_allocVector(STRSXP, n));
  std::string res;
  
  for(int i=0 ; i<n ; ++i){
    const char *path = Rf_translateCharUTF8(STRING_ELT(R_paths, i));
    absolute_to_relative_single(path, root_path, res);
    SET_STRING_ELT(res_all, i, Rf_mkCharCE(res.c_str(), CE_UTF8));
  }
  
  UNPROTECT(1);
  
  return res_all;  
}


inline bool is_chunk_line(std::string line, bool &is_ignore, std::string &chunk_title){
  
  if(line.size() < 6 || line[0] != '#'){
    return false;
  }
  
  const size_t n = line.size();
  
  //
  // does it end with "####"?
  //
  
  size_t i = n - 1;
  while(i >= 0 && line[i] == ' '){
    --i;
  }
  
  if(i < 5 || line.back() != '#'){
    return false;
  }
  
  for(size_t offset = 1 ; offset <= 3 ; ++offset){
    if(line[i - offset] != '#'){
      return false;
    }
  }
  
  size_t i_end = i - 4;
  
  //
  // does it start with "# =" or "# !="?
  //
  
  i = 1;
  // trim #
  while(i < n && line[i] == '#'){
    ++i;
  }
  
  // trim spaces
  while(i < n && line[i] == ' '){
    ++i;
  }
  
  if(i == n){
    return false;
  }
  
  if(line[i] == '='){
    is_ignore = false;
    
    // saving the chunk title, note that empty titles are valid
    while(i_end >= 0 && (line[i_end] == '#' || line[i_end] == ' ')){
      --i_end;
    }
    
    size_t i_start = i + 1;
    chunk_title.clear();
    for(size_t i = i_start ; i <= i_end ; ++i){
      chunk_title += line[i];
    }
    
    return true;
    
  } else if(line[i] == '!' && line[i + 1] == '='){
    is_ignore = true;
    return true;
    
  }
  
  return false;
  
}

// [[Rcpp::export]]
Rcpp::List cpp_create_chunks(SEXP R_path){
  
  if(TYPEOF(R_path) != STRSXP){
    Rcpp::stop("cpp_create_chunks: The argument `R_path` must be a character scalar. Currently the type is wrong.");
  }
  
  std::string path_str = Rf_translateCharUTF8(STRING_ELT(R_path, 0));
  
  fs::path path = path_str;
  
  if(!fs::exists(path)){
    Rcpp::stop("cpp_create_chunks: You must provide an existing path. The following path does not exist:\n" + path_str);
  }
  
  std::ifstream file_in(path);
  
  Rcpp::List res;
  
  if(!file_in.is_open()){
    return res;
  }
  
  // Implementation notes:
  // - we do not handle the case where "# = ####" is placed within a string
  // => later I should catch this case
  
  std::vector<std::string> current_chunk;
  std::string current_chunk_title;
  std::string new_chunk_title;
  
  bool in_chunk = false;
  bool is_ignore = false;
  std::string line;
  while(std::getline(file_in, line)){
    
    if(is_chunk_line(line, is_ignore, new_chunk_title)){
      // we save the previous chunk
      if(in_chunk){
        res.push_back(current_chunk, current_chunk_title);
        current_chunk.clear();
      }
      
      if(is_ignore){
        in_chunk = false;
      } else {
        in_chunk = true;
        current_chunk_title = new_chunk_title;
      }
      
    } else if(in_chunk){
      current_chunk.push_back(line);
      
    }
    
  }
  
  if(in_chunk){
    res.push_back(current_chunk, current_chunk_title);
  }
  
  
  file_in.close();
  
  return res;
}

