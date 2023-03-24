abstract type LasRecordData end

"""
A LAS "variable length record" - the generic way to store extra user or
organization defined binary metadata in LAS files.
"""
struct LasVariableLengthRecord{T<:LasRecordData}
    description::AbstractString
    data::T
end

# size of a VLR in bytes
# assumes it is not extended VLR
lassize(vlr::LasVariableLengthRecord) = 54 + length(vlr.data.raw_data)

struct LasfProjectionData{ID} <: LasRecordData
    raw_data::Vector{UInt8}
end

struct LasfSpecData{ID} <: LasRecordData
    raw_data::Vector{UInt8}
end

struct GenericRecordData <: LasRecordData
    user_id::AbstractString
    record_id::UInt16
    raw_data::Vector{UInt8}
end

Base.summary(vlr::LasVariableLengthRecord) = "VLR of size $(lassize(vlr))"
Base.summary(vlr::LasVariableLengthRecord{T}) where {ID, T <: LasfProjectionData{ID}} =
    "LASF_Projection($ID) of size $(lassize(vlr))"
Base.summary(vlr::LasVariableLengthRecord{T}) where {ID, T <: LasfSpecData{ID}} =
    "LASF_Spec($ID) of size $(lassize(vlr))"
Base.summary(vlr::LasVariableLengthRecord{GenericRecordData}) =
    "$(vlr.data.user_id)($(vlr.data.record_id)) of size $(lassize(vlr))"

# Read a variable length metadata record from a stream.
#
# If `extended` is true, the VLR is one of the extended VLR types specified in
# the LAS 1.4 spec which can be larger and come after the point data.
function Base.read(io::IO, ::Type{LasVariableLengthRecord};
        extended::Bool=false, version=v"1.4")

    # read fields according to table 6 of specification 1.4r15
    reserved = read(io, UInt16)
    user_id = readstring(io, 16)
    record_id = read(io, UInt16)
    record_data_length::Int = extended ? read(io, UInt64) : read(io, UInt16)
    description = readstring(io, 32)

    # check value of reserved field
    if version >= v"1.4"
        iszero(reserved) || @warn "Reserved field in VLR header is non-zero"
    elseif version == v"1.0"
        reserved == 0xAABB || @warn "Reserved field in VLR header is not 0xAABB"
    else
        # standards 1.1–1.3 do not specify the reserved field, but having a
        # value other than those two is unexpected enough to warrant a warning
        iszero(reserved) || reserved == 0xAABB ||
            @warn "Reserved field in VLR header has non-standard value $reserved"
    end

    raw_data = read(io, record_data_length)
    data = if user_id == "LASF_Projection"
        LasfProjectionData{record_id}(raw_data)
    elseif user_id == "LASF_Spec"
        LasfSpecData{record_id}(raw_data)
    else
        GenericRecordData(user_id, record_id, raw_data)
    end

    LasVariableLengthRecord(description, data)
end

function Base.write(io::IO, vlr::LasVariableLengthRecord, extended::Bool=false)
    write(io, vlr.reserved)
    writestring(io, vlr.user_id, 16)
    write(io, vlr.record_id)
    record_data_length = extended ? UInt64(sizeof(vlr.data)) : UInt16(sizeof(vlr.data))
    write(io, record_data_length)
    writestring(io, vlr.description, 32)
    write(io, vlr.data)
    nothing
end
