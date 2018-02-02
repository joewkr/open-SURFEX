function(tidy_source source_list_name)
    set(source ${${source_list_name}})

    # Remove comments and unwanted spaces
    set(result "")
    foreach(line ${source})
        string(REGEX REPLACE "!.*$" "" line "${line}")

        string(STRIP "${line}" line)
        string(TOUPPER "${line}" line)

        string(LENGTH "${line}" line_length)
        if(line_length GREATER 0)
            list(APPEND result ${line})
        endif()
    endforeach(line)
    set(source ${result} PARENT_SCOPE)
endfunction(tidy_source)

function(join_lines source_list_name)
    set(source ${${source_list_name}})

    # Join split lines
    set(result "")
    set(restored_line "")

    foreach(line ${source})
        string(REGEX MATCH "&$" match "${line}")
        string(REGEX REPLACE "&$" "" line "${line}")
        string(REGEX REPLACE "^&" "" line "${line}")

        set(restored_line "${restored_line} ${line}")
        if(NOT match)
            string(STRIP "${restored_line}" restored_line)
            list(APPEND result ${restored_line})
            set(restored_line "")
        endif()
    endforeach(line)
    set(source ${result} PARENT_SCOPE)
endfunction(join_lines)

function(prepare_input file_name_arg_name output)
    # Read file and prepare it for processing
    set(file_name ${${file_name_arg_name}})

    set(program    "[pP][rR][oO][gG][rR][aA][mM]")
    set(module     "[mM][oO][dD][uU][lL][eE]")
    set(subroutine "[sS][uU][bB][rR][oO][uU][tT][iI][nN][eE]")
    set(function   "[fF][uU][nN][cC][tT][iI][oO][nN]")
    set(end        "[eE][nN][dD]")
    file(STRINGS ${file_name} source REGEX
        "^[ \t]*(${end})?[ \t]*(${program}|${module}|${subroutine}|${function})")

    #file(READ ${file_name} source)
    #string(REGEX REPLACE ";" "@@@COMMA@@@" source "${source}")
    #string(REGEX REPLACE "\r?\n" ";" source "${source}")

    # Replace square brackets since they have a special meaning in cmake
    string(REGEX REPLACE "\\[" "@@@LBRA@@@" source "${source}")
    string(REGEX REPLACE "\\]" "@@@RBRA@@@" source "${source}")

    tidy_source(source)
    #join_lines(source)

    set(${output} ${source} PARENT_SCOPE)
endfunction(prepare_input)

function(list_top_level_routines file_name output has_main_program)
    # Get list of top-level routines

    message(STATUS "Inspecting ${file_name}")
    prepare_input(file_name source)

    set(current_unit "")
    set(current_unit_name "")
    set(inside FALSE)

    set(top_level_routines "")
    set(found_main_program FALSE)
    foreach(line ${source})
        string(REGEX MATCH "^(PROGRAM|MODULE|SUBROUTINE|FUNCTION)[ \t]+([A-Z0-9_]+)" match "${line}")
        if(match)
            if(NOT inside)
                set(current_unit ${CMAKE_MATCH_1})
                set(current_unit_name ${CMAKE_MATCH_2})
                set(inside TRUE)
            endif()
        else()
            string(REGEX MATCH "^END[ \t]*${current_unit}[ \t]+${current_unit_name}$" match_end "${line}")
            if(match_end)
                set(inside FALSE)
                if(NOT (current_unit MATCHES "(MODULE|PROGRAM)"))
                    string(TOLOWER "${current_unit_name}" current_unit_name)
                    list(APPEND top_level_routines "${current_unit_name}")
                endif()
                if(current_unit MATCHES "PROGRAM")
                    set(found_main_program TRUE)
                endif()
            endif()
        endif()
    endforeach(line)

    if(inside)
        message(FATAL_ERROR "Unable to find: END ${current_unit} ${current_unit_name}")
    endif()

    set(${output} "${top_level_routines}" PARENT_SCOPE)
    set(${has_main_program} "${found_main_program}" PARENT_SCOPE)
endfunction(list_top_level_routines)

function(find_top_level_routines sources)
    set(naked_routines "")
    set(problematic_sources "")
    set(main_progams "")
    foreach(file ${sources})
        list_top_level_routines(${file} routines has_main_program)
        if(routines)
            list(APPEND problematic_sources "${file}")
            string(REPLACE ";" " " routines "${routines}")
            list(APPEND naked_routines  "${routines}")
        endif()

        if(has_main_program)
            list(APPEND main_progams "${file}")
        endif()
    endforeach(file)

    list(LENGTH problematic_sources num_problems)
    if(num_problems GREATER 0)
        message(STATUS "Found naked top-level routine(s):")
        math(EXPR num_problems "${num_problems} - 1")
        foreach(item RANGE "${num_problems}")
            list(GET problematic_sources ${item} current_file)
            list(GET naked_routines      ${item} current_naked_routines)

            message(STATUS "${current_file}: ${current_naked_routines}")
        endforeach(item)
        message(STATUS "Please move them to corresponding modules.")
        message(FATAL_ERROR "Unable to proceed")
    endif()

    if(ARGC GREATER 1)
        set(${ARGV1} "${main_progams}" PARENT_SCOPE)
    endif()
endfunction(find_top_level_routines)
