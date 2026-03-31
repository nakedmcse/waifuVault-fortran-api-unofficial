! Waifuvault Fortran Mocks
module waifuvault_mocks
    implicit none
    use http_callback
    type, public :: dispatch_mock_type
        integer :: calls
        integer :: http_code
        character(len=:), allocatable :: target_url
        character(len=:), allocatable :: target_method
        character(len=:), allocatable :: fields
        type(response_type) :: response
        contains
        procedure clear_dispatch_mock
    end type dispatch_mock_type

    type(dispatch_mock_type) :: dispatch_mock

    ! Responses
    character(len=*), parameter :: response_bad_request = "{\"name\": \"BAD_REQUEST\", \"message\": \"Error Test\", \"status\": 400}"
    character(len=*), parameter :: response_fileInfoOK = "{\"url\":\"https://waifuvault.moe/f/something\", \"token\":\"test-token\", \"bucket\":\"test-bucket\", \"retentionPeriod\":100, \"options\":{\"protected\":false, \"hideFilename\":false, \"oneTimeDownload\":false}}"
    character(len=*), parameter :: response_fileInfoOKText = "{\"url\":\"https://waifuvault.moe/f/something\", \"token\":\"test-token\", \"bucket\":\"test-bucket\", \"retentionPeriod\":\"10 minutes\", \"options\":{\"protected\":false, \"hideFilename\":false, \"oneTimeDownload\":false}}"
    character(len=*), parameter :: response_delete_true = "true"
    character(len=*), parameter :: response_empty_bucket = "{\"token\":\"test-bucket\", \"files\":[]}"
    character(len=*), parameter :: response_used_bucket = "{\"token\":\"56a62473-d3ef-48f9-baef-3628a3d23549\",\"files\":[{\"bucket\":\"56a62473-d3ef-48f9-baef-3628a3d23549\",\"retentionPeriod\":null,\"album\":null,\"token\":\"0dd4b9b5-1e7e-4852-bdc5-54a79feb07c9\",\"id\":21343,\"views\":13,\"url\":\"https://waifuvault.moe/f/d270ad3d-3992-4dec-9ddd-ee32c6f5706f/voice.zip\",\"options\":{\"hideFilename\":false,\"oneTimeDownload\":false,\"protected\":false}},{\"bucket\":\"56a62473-d3ef-48f9-baef-3628a3d23549\",\"retentionPeriod\":20905635650,\"album\":{\"token\":\"b96413f7-2e34-4691-8f44-6b9fcf83ca7c\",\"publicToken\":\"ce8c7459-b26f-4844-b65a-4d1668308c8e\",\"name\":\"Something\",\"bucket\":\"56a62473-d3ef-48f9-baef-3628a3d23549\",\"dateCreated\":1766428873426},\"token\":\"bb183720-58eb-44d6-9eff-d72536edf302\",\"id\":21084,\"views\":0,\"url\":\"https://waifuvault.moe/f/0b62bc0d-f0fc-471f-aacb-f9e35b0e6821/having%20an%20excited%20conversation%20over%20tea%20in%20a%20victorian%20setting%20s-1073058833.png\",\"options\":{\"hideFilename\":false,\"oneTimeDownload\":false,\"protected\":false}}],\"albums\":[{\"token\":\"b96413f7-2e34-4691-8f44-6b9fcf83ca7c\",\"publicToken\":\"ce8c7459-b26f-4844-b65a-4d1668308c8e\",\"name\":\"Something\",\"bucket\":\"56a62473-d3ef-48f9-baef-3628a3d23549\",\"dateCreated\":1766428873426}]}"
    character(len=*), parameter :: response_album_new = "{\"token\": \"test-album\", \"bucketToken\":\"test-bucket\", \"publicToken\":null, \"name\":\"test-name\", \"files\":[]}"
    character(len=*), parameter :: response_album_with_files = "{\"token\":\"b96413f7-2e34-4691-8f44-6b9fcf83ca7c\",\"bucketToken\":\"56a62473-d3ef-48f9-baef-3628a3d23549\",\"publicToken\":\"ce8c7459-b26f-4844-b65a-4d1668308c8e\",\"name\":\"Something\",\"dateCreated\":1766428873426,\"files\":[{\"bucket\":\"56a62473-d3ef-48f9-baef-3628a3d23549\",\"retentionPeriod\":20841380227,\"album\":null,\"token\":\"bb183720-58eb-44d6-9eff-d72536edf302\",\"id\":21084,\"views\":0,\"url\":\"https://waifuvault.moe/f/0b62bc0d-f0fc-471f-aacb-f9e35b0e6821/having%20an%20excited%20conversation%20over%20tea%20in%20a%20victorian%20setting%20s-1073058833.png\",\"options\":{\"hideFilename\":false,\"oneTimeDownload\":false,\"protected\":false}},{\"bucket\":\"56a62473-d3ef-48f9-baef-3628a3d23549\",\"retentionPeriod\":null,\"album\":null,\"token\":\"49cc14d8-c4da-410a-91f7-09848f1e8466\",\"id\":22185,\"views\":0,\"url\":\"https://waifuvault.moe/f/b9d1f463-5f41-49cb-980b-ca3043382634/1999.jpg\",\"options\":{\"hideFilename\":false,\"oneTimeDownload\":false,\"protected\":false}}]}"
    character(len=*), parameter :: response_general_true = "{\"success\":true, \"description\":\"yes\"}"
    character(len=*), parameter :: response_fileStatsResponse = "{\"recordCount\": 2, \"recordSize\":100}"
    character(len=*), parameter :: response_restrictionsResponse = "[{\"type\": \"MAX_FILE_SIZE\",\"value\": 100},{\"type\": \"BANNED_MIME_TYPE\",\"value\": \"application/x-msdownload,application/x-executable\"}]"

    contains
        subroutine clear_dispatch_mock(this)
            class(dispatch_mock) :: this
            this%calls = 0
            this%http_code = 200
            this%target_url = ""
            this%target_method = ""
            this%fields = ""
            this%response%content = ""
        end subroutine clear_dispatch_mock
end module waifuvault_mocks