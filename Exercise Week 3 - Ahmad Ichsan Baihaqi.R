### Exercise Week 3

### Author: Ahmad Ichsan Baihaqi
### Email: ahmadichsanbaihaqi@gmail.com

question_one = function() {
  print("QUESTION ONE")
  
  substring_count = function(input_string, key) {
    counter = 0
    
    key_length = length(strsplit(key, "")[[1]])
    
    splitted_input = strsplit(input_string, "")[[1]]
    splitted_length = length(splitted_input)
    
    limit = splitted_length - 1
    
    for (i in 0:limit) {
      start = i
      end = start + key_length - 1
      content = substr(input_string, start, end)
      
      if (content == key) {
        counter = counter + 1
      }
    }
    
    return (counter)
  }
  
  printing = function(print_key, print_input, print_result) {
    print(paste0(print_key, " occurance in ", print_input, ' is ', print_result))
  }
  
  # Output: 2
  input1 = 'ABCDCDC'
  key1 = 'CD'
  result1 = substring_count(input1, key1)
  printing(key1, input1, result1)
  
  # Output: 2
  input2 = 'ABCDCDC'
  key2 = 'CDC'
  result2 = substring_count(input2, key2)
  printing(key2, input2, result2)
  
  # Output: 3
  input3 = 'DEDFDEDEDEG'
  key3 = 'DED'
  result3 = substring_count(input3, key3)
  printing(key3, input3, result3)
}

question_one()


print('==============================================')

question_two = function() {
  print("QUESTION TWO")
  
  # use recursive
  diff_letters = function(input, diff) {
    splitted_input = strsplit(input, "")[[1]]
    
    for (item in splitted_input) {
      filterred = splitted_input[splitted_input != item]
      
      diff = diff + 1
      
      if (length(filterred) != 0) {
        diff = diff_letters(paste(filterred, collapse = ''), diff)
      }
      
      break
    }
    
    return (diff)
  }
  
  printing = function(print_input, print_result) {
    print(paste0("different letters in ", print_input, ' is ', print_result))
  }
  
  # Output: 2
  input1 = 'AAAAAAAAAB'
  result1 = diff_letters(input1, 0)
  printing(input1, result1)
  
  # Output: 5
  input2 = 'ABCDAAAABBCCCE'
  result2 = diff_letters(input2, 0)
  printing(input2, result2)
}

question_two()


print('==============================================')


question_three = function() {
  print("QUESTION THREE")
  
  distance = function(input) {
    extracted_list = unlist(regmatches(input, gregexpr("\\-*\\d+\\.*\\d*", input)))
    
    x1 = strtoi(extracted_list[1])
    y1 = strtoi(extracted_list[2])
    x2 = strtoi(extracted_list[3])
    y2 = strtoi(extracted_list[4])
    
    first_point = x1 - x2
    first_point_square = first_point ** 2
    
    second_point = y1 - y2
    second_point_square = second_point ** 2
    
    result = (first_point_square + second_point_square) ** 0.5
    return (result)
    
  }
  
  # Output: 4.242640687119285
  input1 = '(0,-1), (-3,2)'
  result1 = distance(input1)
  print(paste0('The distance of ', input1, ' is ', result1))
}

question_three()
