function(tidy_source source_list_name)
    set(source ${${source_list_name}})

    #string(TIMESTAMP time)
    #message(STATUS "TIDY_SOURCE ${time}")
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

    #string(TIMESTAMP time)
    #message(STATUS "JOIN_LINES ${time}")
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

    # Read file contents ignoring empty lines
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

    if(source MATCHES "^!CMAKE: SKIP NO GEN INTERFACES")
        message(STATUS "Skipping ${file_name} due to user-defined SKIP flag;")
        message(STATUS "assuming that there aren't any top-level routines that")
        message(STATUS "require auto-generated interfaces.")
        message(STATUS "To inspect this file for required interfaces, please")
        message(STATUS "remove its first line: '!CMAKE: SKIP NO GEN INTERFACES'")
    else()
        # Replace square brackets since they have a special meaning in cmake
        string(REGEX REPLACE "\\[" "@@@LBRA@@@" source "${source}")
        string(REGEX REPLACE "\\]" "@@@RBRA@@@" source "${source}")

        tidy_source(source)
        #join_lines(source)

        set(${output} ${source} PARENT_SCOPE)
    endif()
endfunction(prepare_input)

function(list_top_level_routines file_name output)
    # Get list of top-level routines

    message(STATUS "Inspecting ${file_name}")
    prepare_input(file_name source)

    string(TIMESTAMP time)
    #message(STATUS "select  ${time}")

    set(current_unit "")
    set(current_unit_name "")
    set(inside FALSE)

    set(result "")
    foreach(line ${source})
        string(REGEX MATCH "^(PROGRAM|MODULE|SUBROUTINE|FUNCTION)[ \t]+([A-Z0-9_]+)" match "${line}")
        if(match)
            if(NOT inside)
                set(current_unit ${CMAKE_MATCH_1})
                set(current_unit_name ${CMAKE_MATCH_2})
                set(inside TRUE)
            endif()
        else()
            string(REGEX MATCH "^END ${current_unit} ${current_unit_name}$" match_end "${line}")
            if(match_end)
                set(inside FALSE)
                if(NOT (current_unit MATCHES "(MODULE|PROGRAM)"))
                    string(TOLOWER "${current_unit_name}" current_unit_name)
                    list(APPEND result "${current_unit_name}")
                endif()
            endif()
        endif()
    endforeach(line)

    if(inside)
        message(FATAL_ERROR "Unable to find: END ${current_unit} ${current_unit_name}")
    endif()

    set(${output} "${result}" PARENT_SCOPE)
endfunction(list_top_level_routines)

function(find_top_level_routines output sources)
    set(naked_routines "")
    set(problematic_sources "")
    foreach(file ${sources})
        list_top_level_routines(${file} routines)
        if(routines)

            set(result "")
            #foreach(item ${routines})
            #    string(APPEND result " ${output_dir}/${item}")
            #endforeach(item)
            #set(routines "${result}")
#
            #file(APPEND ${output} "\"${file} ${routines}\"\n")

            list(APPEND problematic_sources "${file}")
            string(REPLACE ";" " " routines "${routines}")
            list(APPEND naked_routines  "${routines}")
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
endfunction(find_top_level_routines)

function(generate_interfaces file_name output)
    # Generate interfaces for all top-level routines
    message(STATUS "inspecting ${file_name}")
    prepare_input(file_name source)

    set(current_unit "")
    set(current_unit_name "")

    set(inside FALSE)
    set(import_part FALSE)
    set(decl_part FALSE)
    set(skip_unit FALSE)

    set(unit_decl "")

    set(modules "")
    set(imported "")

    set(definitions "")
    set(variables "")

    foreach(line ${source})
        string(REGEX MATCH "^#" match "${line}")
        if(match)
            continue()
        endif()

        string(REGEX MATCH "^INCLUDE[ \t]*['\"]" match "${line}")
        if(match)
            continue()
        endif()

        string(REGEX MATCH "^(PROGRAM|MODULE|SUBROUTINE|FUNCTION)[ \t]+([A-Z0-9_]+)" match "${line}")
        if(match)
            if(NOT inside)
                set(current_unit ${CMAKE_MATCH_1})
                set(current_unit_name ${CMAKE_MATCH_2})

                set(unit_decl ${line})
                string(REGEX REPLACE "[ \t]+" " " unit_decl ${unit_decl})

                set(inside TRUE)
                set(import_part TRUE)
                if(current_unit MATCHES "(MODULE|PROGRAM)")
                    set(skip_unit TRUE)
                endif()

                continue()
            endif()
        endif()

        if(inside AND import_part AND NOT skip_unit)
            string(REGEX MATCH "^USE[ \t]+([A-Z0-9_]+)" match "${line}")
            if(match)
                set(module_name ${CMAKE_MATCH_1})

                # Require explicit import list to be present
                string(REGEX MATCH "${module_name}[ \t]*,[ \t]*ONLY[ \t]*:" match ${line})
                if(match)
                    string(REGEX REPLACE "^.+:" "" imported_raw ${line})
                    string(REGEX REPLACE "," ";" imported_raw ${imported_raw})

                    foreach(item ${imported_raw})
                        string(REGEX REPLACE "=>.+" "" item ${item})
                        string(STRIP "${item}" item)
                        list(APPEND imported ${item})

                        list(APPEND modules ${line})
                    endforeach(item)
                endif()

            continue()
            endif()
        endif()

        # Explicit IMPLICIT
        if(inside AND NOT skip_unit)
            string(REGEX MATCH "^IMPLICIT" match "${line}")
            if(match)
                set(import_part FALSE)
                set(decl_part TRUE)

                continue()
            endif()
        endif()

        if(inside AND decl_part AND NOT skip_unit)
            string(REGEX MATCH
                "^(TYPE|CLASS|CHARACTER|INTEGER|REAL|LOGICAL|COMPLEX|DOUBLE[ \t]*PRECISION)"
                match
                "${line}")
            if(match)
                string(REGEX MATCH "::" match ${line})
                if(match)
                    string(REGEX REPLACE "^.+::" "" vars_raw ${line})

                    string(REGEX REPLACE "[ \t]*::.+$" "" decl_raw ${line})
                else()
                    string(REGEX REPLACE "^[A-Z0-9_]+" "" vars_raw ${line})

                    string(REGEX MATCH "^([A-Z0-9_]+)" match ${line})
                    message("${match}, ${CMAKE_MATCH_1}")
                    set(decl_raw ${CMAKE_MATCH_1})
                endif()

                string(REGEX MATCHALL "[A-Z0-9_]+([ \t]*\\([^\\)]+\\))?" vars_raw ${vars_raw})

                foreach(item ${vars_raw})
                    string(STRIP "${item}" item)
                    set(item_raw "${item}")

                    string(REGEX MATCH "^([A-Z0-9_]+)" match ${item})
                    set(item ${CMAKE_MATCH_1})

                    string(LENGTH "${item}" item_length)

                    if(item_length GREATER 0)
                        list(APPEND variables ${item})
                        list(APPEND definitions "${decl_raw} :: ${item_raw}")
                    endif()
                endforeach(item)

                continue()
            else()
                set(decl_part FALSE)
            endif()
        endif()

        if(inside)
            string(REGEX MATCH "^END ${current_unit} ${current_unit_name}$" match "${line}")
            if(match)
                if(NOT skip_unit)
                    set(arguments "")

                    string(REGEX MATCH "${current_unit}[ \t]+${current_unit_name}[ \t]*\\(([A-Z0-9_, \t]+)\\)" match ${unit_decl})
                    if(match)
                        set(arguments_raw ${CMAKE_MATCH_1})
                        string(REGEX REPLACE "," ";" arguments_raw "${arguments_raw}")
                        foreach(item ${arguments_raw})
                            string(STRIP "${item}" item)
                            list(APPEND arguments "${item}")
                        endforeach(item)
                    endif()
                    if(current_unit MATCHES "FUNCTION")
                        string(REGEX MATCH "RESULT[ \t]*\\([ \t]*([A-Z0-9_]+)[ \t]*\\)" match ${unit_decl})
                        if(match)
                            list(APPEND arguments "${CMAKE_MATCH_1}")
                        elseif(unit_decl MATCHES "^FUNCTION")
                            list(APPEND arguments "${current_unit_name}")
                        endif()
                    endif()

                    set(arguments_decls "")
                    foreach(item ${arguments})
                        list(FIND variables ${item} index)
                        if(index GREATER -1)
                            list(GET definitions ${index} current_def)
                            list(APPEND arguments_decls ${current_def})
                        else()
                            string(REGEX REPLACE ";" " " variables "${variables}")
                            string(TOLOWER ${current_unit_name} current_unit_name_l)
                            message(STATUS "failed to generate ${output}/modi_${current_unit_name_l}.f90")
                            message(FATAL_ERROR
                                "Unable to find declaration for ${item} in ${variables}")
                        endif()
                    endforeach(item)

                    set(reqired_modules "")
                    foreach(item ${arguments_decls})
                        string(REGEX MATCH "(TYPE|CLASS)[ \t]*\\([ \t]*([A-Z0-9_]+)[ \t]*\\)" match "${item}")

                        if(match)
                            set(imported_type ${CMAKE_MATCH_2})
                            list(FIND imported ${imported_type} index)
                            if(index GREATER -1)
                                list(GET modules ${index} current_module)
                                list(APPEND reqired_modules ${current_module})
                            else()
                                message(FATAL_ERROR "Unable to find explicit import for: ${imported_type}")
                            endif()
                        endif()
                    endforeach(item)

                    foreach(item ${arguments_decls})
                        string(REGEX MATCH "KIND[ \t]*=[ \t]*([A-Z0-9_]+)" match "${item}")

                        if(match)
                            set(imported_type ${CMAKE_MATCH_1})
                            list(FIND imported ${imported_type} index)
                            if(index GREATER -1)
                                list(GET modules ${index} current_module)
                                list(APPEND reqired_modules ${current_module})
                            else()
                                message(FATAL_ERROR "Unable to find explicit import for: ${imported_type}")
                            endif()
                        endif()
                    endforeach(item)

                    string(TOLOWER ${current_unit_name} current_unit_name_l)

                    message(STATUS "generating ${output}/modi_${current_unit_name_l}.f90")

                    set(output_file "${output}/modi_${current_unit_name_l}.f90")
                    file(WRITE ${output_file} "! AUTO-GENERATED INTERFACE FOR ${file_name}\n")
                    file(APPEND ${output_file} "MODULE MODI_${current_unit_name}\n\n")
                    file(APPEND ${output_file} "INTERFACE\n" )
                    file(APPEND ${output_file} "${unit_decl}\n")
                    foreach(item ${reqired_modules})
                        file(APPEND ${output_file} "${item}\n")
                    endforeach(item)
                    file(APPEND ${output_file} "IMPLICIT NONE\n")
                    foreach(item ${arguments_decls})
                        file(APPEND ${output_file} "${item}\n")
                    endforeach(item)
                    file(APPEND ${output_file} "END ${current_unit} ${current_unit_name}\n")
                    file(APPEND ${output_file} "END INTERFACE\n\n" )
                    file(APPEND ${output_file} "END MODULE MODI_${current_unit_name}\n" )

                endif()
                # Return to initial state
                set(inside FALSE)
                set(import_part FALSE)
                set(decl_part FALSE)
                set(skip_unit FALSE)

                set(unit_decl "")
                set(modules "")
                set(imported "")
                set(definitions "")
                set(variables "")
            endif()
        endif()
    endforeach(line)

    if(inside)
        message(FATAL_ERROR "Unable to find: END ${current_unit} ${current_unit_name}")
    endif()

endfunction(generate_interfaces)

if(STANDALONE)
    generate_interfaces(${FILE_NAME} ${GENERATED_ROOT})
endif()