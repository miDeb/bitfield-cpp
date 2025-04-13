#include <cassert>
#include <cstdint>
#include <type_traits>
#include <cstddef>

/**
 * contains implementation details of the bitfield implementation
 */
namespace bitfield_detail {
    /**
     * The smallest type out of uint8_t - uint64_t that can fit this amount of bits.
     *
     * @tparam Bits The amount of bits that have to fit.
     */
    template<size_t Bits>
    using MinimumType =
    std::conditional_t<(Bits == 0), void,
        std::conditional_t<(Bits <= 8), uint8_t,
            std::conditional_t<(Bits <= 16), uint16_t,
                std::conditional_t<(Bits <= 32), uint32_t,
                    std::conditional_t<(Bits <= 64), uint64_t,
                        void> > > > >;

    /**
     * Performs ceiling division.
     *
     * @param dividend Dividend.
     * @param divisor Divisor. UB if 0.
     * @return
     */
    constexpr size_t ceiling_div(const size_t dividend, const size_t divisor) {
        assert(divisor != 0);
        return dividend / divisor + (dividend % divisor != 0);
    }

    /**
     * Copies bits from src (offset by src_offset bits) to dst.
     *
     * src and dst must not overlap.
     * @param src Must point to a buffer of at least ceil((src_offset + bits) / 8) bytes.
     * @param dst Must point to a buffer of at least ceil(bits / 8) bytes.
     * @param src_offset The bit offset for src.
     * @param bits The number of bits to copy.
     */
    void bitwise_memcpy_offset_src(const uint8_t *src, uint8_t *dst, size_t src_offset, size_t bits) {
        if (bits == 0) {
            return;
        }
        // skip bytes at the beginning
        src += src_offset / 8;
        src_offset %= 8;

        // The number of bytes we have to write into
        const size_t dst_bytes = ceiling_div(bits, 8);
        // The number of bytes we have to read from
        const uint8_t src_bytes = ceiling_div(bits + src_offset, 8);

        // Which bits of the last byte do we have to touch?
        const size_t last_byte_bit_count = bits % 8;
        const uint8_t last_byte_mask = (last_byte_bit_count == 0) ? 0xFF : ((1U << last_byte_bit_count) - 1);

        for (size_t i = 0; i < dst_bytes; ++i) {
            uint8_t value = src[i] >> src_offset;
            uint8_t mask = 0xff;
            if (i == dst_bytes - 1) {
                // This is the last byte
                mask = last_byte_mask;
            }
            if (i < src_bytes - 1 && src_offset != 0) {
                // Grab bits from the next byte if necessary
                value |= src[i + 1] << (8 - src_offset);
            }

            dst[i] = (dst[i] & ~mask) | (value & mask);
        }
    }


    /**
     * Copies bits from src to dst (offset by dst_offset bits).
     *
     * src and dst must not overlap.
     * @param src Must point to a buffer of at least ceil(bits / 8) bytes.
     * @param dst Must point to a buffer of at least ceil((src_offset + bits) / 8) bytes.
     * @param dst_offset The bit offset for dst.
     * @param bits The number of bits to copy.
     */
    void bitwise_memcpy_offset_dst(const uint8_t *src, uint8_t *dst, size_t dst_offset, size_t bits) {
        if (bits == 0) {
            return;
        }
        // skip bytes at the beginning
        dst += dst_offset / 8;
        dst_offset %= 8;

        // The number of bytes we have to write into
        const size_t dst_bytes = ceiling_div(bits + dst_offset, 8);
        // The number of bytes we have to read from
        const size_t src_bytes = ceiling_div(bits, 8);

        // Which bits of the first byte do we have to touch?
        const uint8_t first_byte_mask = 0xFF << dst_offset;

        // Which bits of the last byte do we have to touch?
        const size_t last_byte_bit_count = (dst_offset + bits) % 8;
        const uint8_t last_byte_mask = (last_byte_bit_count == 0) ? 0xFF : ((1U << last_byte_bit_count) - 1);

        for (size_t i = 0; i < dst_bytes; ++i) {
            // avoid reading from src if we don't need bits from src[i]
            uint8_t value =
                    i < src_bytes ? src[i] << dst_offset : 0;
            uint8_t mask = 0xff;
            if (i == 0) {
                // This is the first byte
                mask &= first_byte_mask;
            }
            if (i == dst_bytes - 1) {
                // This is the last byte
                mask &= last_byte_mask;
            }
            if (i != 0 && dst_offset != 0) {
                // Grab bits from the previous byte if necessary
                value |= src[i - 1] >> (8 - dst_offset);
            }

            dst[i] = (dst[i] & ~mask) | (value & mask);
        }
    }
}

/**
 * A class intended to be used in a union to define bitfields.
 * @tparam Index The first bit that belongs to this field.
 * @tparam Bits The number of bits that belong to this field.
 */
template<size_t Index, size_t Bits>
class Bitfield {
    static_assert(Bits <= 64);

    using T = bitfield_detail::MinimumType<Bits>;

public:
    Bitfield() = default;

    Bitfield &operator=(T value) {
        auto *src = reinterpret_cast<const uint8_t *>(&value);
        bitfield_detail::bitwise_memcpy_offset_dst(src, value_, Index, Bits);
        return *this;
    }

    explicit operator T() const {
        T value = 0;
        auto *dest = reinterpret_cast<uint8_t *>(&value);
        bitfield_detail::bitwise_memcpy_offset_src(value_, dest, Index, Bits);
        return value;
    }

    [[nodiscard]] T val() {
        return T(*this);
    }

    // --- Prevent copy/move construction/assignment ---
    // If copying/moving the value is desired, the value should first be extracted.
    // This class only really makes sense in a union.

    Bitfield(const Bitfield&) = delete;
    Bitfield(Bitfield&&) = delete;
    Bitfield& operator=(const Bitfield&) = delete;
    Bitfield& operator=(Bitfield&&) = delete;

private:
    uint8_t value_[bitfield_detail::ceiling_div(Index + Bits, 8)] = {};
};

union X {
    Bitfield<0, 2> a;
    Bitfield<2, 1> b;
    Bitfield<3, 4> c;
};

int main() {
    X x{};

    assert(x.a.val() == 0);
    assert(x.b.val() == 0);
    assert(x.c.val() == 0);

    x.a = 2;
    assert(x.a.val() == 2);
    assert(x.b.val() == 0);
    assert(x.c.val() == 0);

    x.c = 5;
    assert(x.a.val() == 2);
    assert(x.b.val() == 0);
    assert(x.c.val() == 5);
}
