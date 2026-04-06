! Waifuvault Fortran Deserializers
module waifuvault_deserializers
    use fjson
    use waifuvault_models
    implicit none
    contains
        ! AST to Object functions
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

        function general_response_from_ast(general_ast) result (ret)
            type(json_node) :: general_ast, ret_ast
            type(general_response) :: ret
            if(general_ast%node_type == "OBJECT") then
                ret_ast = get_node(general_ast,".success")
                ret%success = ret_ast%value_bool
                ret_ast = get_node(general_ast,".description")
                ret%description = ret_ast%value_string
            else
                ret%success = .false.
                ret%description = ''
            end if
        end function general_response_from_ast

        function album_response_from_ast(album_ast) result (res)
            type(album_response) :: res
            type(album_info) :: file_album
            type(file_options) :: file_options
            type(file_response) :: file_res
            type(json_node) :: album_ast, files_ast, ret_ast
            integer :: i
            res%filecount = 0
            allocate(res%files(1))
            if(album_ast%node_type == "OBJECT") then
                ret_ast = get_node(album_ast,".token")
                res%token = ret_ast%value_string
                ret_ast = get_node(album_ast,".publicToken")
                res%publicToken = ret_ast%value_string
                ret_ast = get_node(album_ast,".name")
                res%name = ret_ast%value_string
                ret_ast = get_node(album_ast,".bucketToken")
                res%bucket = ret_ast%value_string
                ret_ast = get_node(album_ast,".dateCreated")
                res%dateCreated = ret_ast%value_int

                files_ast = get_node(album_ast,".files")
                if(files_ast%node_type == "ARRAY" .and. files_ast%child_nodes_count > 0) then
                    do i = 1, files_ast%child_nodes_count
                        file_res = file_response_from_ast(files_ast%child_nodes(i))
                        ret_ast = get_node(files_ast%child_nodes(i),".options")

                        file_res%options = options_from_ast(ret_ast)
                        ret_ast = get_node(files_ast%child_nodes(i),".album")

                        file_res%album = album_info_from_ast(ret_ast)
                        call res%album_append_file(file_res)
                    end do
                else
                    res%filecount = 0
                end if
            else
                res%token = ''
                res%publicToken = ''
                res%name = ''
                res%bucket = ''
                res%dateCreated = 0
                res%filecount = 0
            end if
        end function album_response_from_ast

        function bucket_response_from_ast(bucket_ast) result (res)
            type(bucket_response) :: res
            type(album_info) :: file_album
            type(file_options) :: file_options
            type(file_response) :: file_res
            type(json_node) :: bucket_ast, files_ast, albums_ast, ret_ast
            integer :: i
            res%filecount = 0
            allocate(res%files(1))
            res%albumcount = 0
            allocate(res%albums(1))
            if(bucket_ast%node_type == "OBJECT") then
                ret_ast = get_node(bucket_ast,".token")
                res%token = ret_ast%value_string
                files_ast = get_node(bucket_ast,".files")
                if(files_ast%node_type == "ARRAY" .and. files_ast%child_nodes_count > 0) then
                    do i = 1, files_ast%child_nodes_count
                        file_res = file_response_from_ast(files_ast%child_nodes(i))
                        ret_ast = get_node(files_ast%child_nodes(i),".options")

                        file_res%options = options_from_ast(ret_ast)
                        ret_ast = get_node(files_ast%child_nodes(i),".album")

                        file_res%album = album_info_from_ast(ret_ast)
                        call res%bucket_append_file(file_res)
                    end do
                else
                    res%filecount = 0
                end if

                albums_ast = get_node(bucket_ast,".albums")
                if(albums_ast%node_type == "ARRAY" .and. albums_ast%child_nodes_count > 0) then
                    do i = 1, albums_ast%child_nodes_count
                        file_album = album_info_from_ast(albums_ast%child_nodes(i))
                        call res%bucket_append_album(file_album)
                    end do
                else
                    res%albumcount = 0
                end if
            else
                res%token = ''
                res%filecount = 0
                res%albumcount = 0
            end if
        end function bucket_response_from_ast

        ! High level deserializers
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

        function deserializeGeneralResponse(body) result (res)
            type(general_response) :: res
            type(json_node) :: body_ast
            character(len=*) :: body

            body_ast = parse_json(body)
            if (associated(fjson_error)) then
                print *, "Error parsing general response"
                return
            end if

            res = general_response_from_ast(body_ast)
        end function deserializeGeneralResponse

        function deserializeBucketResponse(body) result (res)
            type(bucket_response) :: res
            type(json_node) :: body_ast
            character(len=*) :: body

            body_ast = parse_json(body)
            if (associated(fjson_error)) then
                print *, "Error parsing bucket response"
                return
            end if

            res = bucket_response_from_ast(body_ast)
        end function deserializeBucketResponse

        function deserializeAlbumResponse(body) result (res)
            type(album_response) :: res
            type(json_node) :: body_ast
            character(len=*) :: body

            body_ast = parse_json(body)
            if (associated(fjson_error)) then
                print *, "Error parsing album response"
                return
            end if

            res = album_response_from_ast(body_ast)
        end function deserializeAlbumResponse
end module waifuvault_deserializers