! Waifuvault Fortran Deserializers
module waifuvault_deserializers
    use fjson
    use waifuvault_models
    implicit none
    contains
        function album_info_from_ast(album_ast) result (album)
            type(json_node) :: album_ast, ret_ast
            type(album_info) :: album
            if(album_ast%node_type == "OBJECT") then
                ret_ast = get_node(album_ast,".token")
                album%token = ret_ast%value_string
                ret_ast = get_node(album_ast,".publicToken")
                album%publicToken = ret_ast%value_string
                ret_ast = get_node(album_ast,".name")
                album%name = ret_ast%value_string
                ret_ast = get_node(album_ast,".dateCreated")
                album%dateCreated = ret_ast%value_int
            else
                album%token = ''
                album%publicToken = ''
                album%name = ''
                album%bucket = ''
                album%dateCreated = -1
            end if
        end function album_info_from_ast

        function options_from_ast(options_ast) result (options)
            type(json_node) :: options_ast, ret_ast
            type(file_options) :: options
            if(options_ast%node_type == "OBJECT") then
                ret_ast = get_node(options_ast,".hideFilename")
                options%hideFilename = ret_ast%value_bool
                ret_ast = get_node(options_ast,".protected")
                options%protected = ret_ast%value_bool
                ret_ast = get_node(options_ast,".oneTimeDownload")
                options%oneTimeDownload = ret_ast%value_bool
            else
                options%hideFilename = .false.
                options%protected = .false.
                options%oneTimeDownload = .false.
            end if
        end function options_from_ast

        function file_response_from_ast(file_ast) result (file)
            type(json_node) :: file_ast, ret_ast
            type(file_response) :: file
            if(file_ast%node_type == "OBJECT") then
                ret_ast = get_node(file_ast,".url")
                file%url = ret_ast%value_string
                ret_ast = get_node(file_ast,".token")
                file%token = ret_ast%value_string
                ret_ast = get_node(file_ast,".bucket")
                file%bucket = ret_ast%value_string
                ret_ast = get_node(file_ast,".retentionPeriod")
                file%retentionPeriod = ret_ast%value_string
                ret_ast = get_node(file_ast,".id")
                file%id = ret_ast%value_int
                ret_ast = get_node(file_ast,".views")
                file%views = ret_ast%value_int
            else
                file%url = ''
                file%token = ''
                file%bucket = ''
                file%retentionPeriod = ''
                file%id = -1
                file%views = -1
            end if
        end function file_response_from_ast

        function deserializeResponse(body) result (res)
            character(len=*) :: body
            type(json_node) :: body_ast, options_ast, album_ast
            type(album_info) :: album
            type(file_options) :: options
            type(file_response) :: res

            body_ast = parse_json(body)
            if (associated(fjson_error)) then
                print *, "Error parsing file response"
                return
            end if

            res = file_response_from_ast(body_ast)

            options_ast = get_node(body_ast,".options")
            res%options = options_from_ast(options_ast)

            album_ast = get_node(body_ast,".album")
            res%album = album_info_from_ast(album_ast)
        end function deserializeResponse
end module waifuvault_deserializers


