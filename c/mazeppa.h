// Prefer writing portable and standard-compliant code. If something can be done
// easily with standard C11, do not resort to GNU C.
//
// Do not use functions and macros prefixed with `mz_priv_` or `MZ_PRIV_` in
// your code: this is private functionality. The rest of the API is public; you
// can use it just like any other library.
//
// By default, Mazeppa checks value tags before performing any operation.
// Unexpected tags cause Mazeppa to print an error message to `stderr` and abort
// the program immediately. You can define `NDEBUG` to disable these checks, but
// then illegal operations like pattern-matching on a string will result in
// undefined behaviour. Consult the language reference for more information on
// what is allowed and what is not.

#ifndef MAZEPPA_H
#define MAZEPPA_H

#ifndef __GNUC__
#error Mazeppa requires GNU language extensions.
#endif

#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef NDEBUG
#define GC_DEBUG
#endif

#include "sds.h"
#include <gc.h>

#define MZ_ENUM_USER_TAGS(...) enum { mz_priv_StartSym = op_F, __VA_ARGS__ }

#define MZ_CALL_MAIN(...) op_main(MZ_PRIV_PTR_TO(mz_Value, __VA_ARGS__))

// Debugging functionality
// /////////////////////////////////////////////////////////////////////////////

// By default, we use `mz_Value`. However, if we need a mutable variable, we
// use `struct mz_value` instead.
typedef const struct mz_value mz_Value;

typedef const struct {
    const char *file, *func;
    int line;
} mz_DebugInfo;

#define MZ_DEBUG_INFO ((mz_DebugInfo){__FILE__, __func__, __LINE__})

typedef const struct {
    mz_DebugInfo debug;
    uint64_t expected_tag;
    const char *expected_tag_name;
} mz_TagMismatch;

#define MZ_TAG_MISMATCH(variant)                                               \
    ((mz_TagMismatch){MZ_DEBUG_INFO, mz_##variant##Tag, #variant})

inline static void mz_stuck(const uint64_t tag, mz_TagMismatch info) {
    mz_TagMismatch i = info;
    fprintf(
        stderr, "%s:%d: %s: Expected tag %" PRIu64 " (`%s`), got %" PRIu64 "\n",
        i.debug.file, i.debug.line, i.debug.func, i.expected_tag,
        i.expected_tag_name, tag);
    abort();
}

// Miscellaneous macros
// /////////////////////////////////////////////////////////////////////////////

#define MZ_PRIV_BOX(T, ...) MZ_PRIV_BOX_MANY(T, 1, __VA_ARGS__)
#define MZ_PRIV_BOX_MANY(T, n, ...)                                            \
    memcpy(                                                                    \
        mz_malloc(n * sizeof(T)), MZ_PRIV_PTR_TO(T, __VA_ARGS__),              \
        n * sizeof(T))

#define MZ_PRIV_PTR_TO(T, ...) ((T *)(T[]){__VA_ARGS__})
#define MZ_PRIV_DEREF(...)     (*(__VA_ARGS__))
#define MZ_PRIV_STRING_SIZE(s)                                                 \
    (sizeof((char[]){s}) - /* the null character */ 1)

#define MZ_PRIV_GEN_SYM(prefix, sym) MZ_PRIV_CAT_4(prefix, sym, _, __COUNTER__)

#define MZ_PRIV_CAT(a, b)      MZ_PRIV_CAT_PRIMITIVE(a, b)
#define MZ_PRIV_CAT_3(a, b, c) MZ_PRIV_CAT(a, MZ_PRIV_CAT(b, c))
#define MZ_PRIV_CAT_4(a, b, c, d)                                              \
    MZ_PRIV_CAT(a, MZ_PRIV_CAT(b, MZ_PRIV_CAT(c, d)))
#define MZ_PRIV_CAT_PRIMITIVE(a, b) a##b

#define MZ_PRIV_STRINGIFY(...)           MZ_PRIV_STRINGIFY_PRIMITIVE(__VA_ARGS__)
#define MZ_PRIV_STRINGIFY_PRIMITIVE(...) #__VA_ARGS__

#define MZ_PRIV_ID(...) __VA_ARGS__

#define MZ_PRIV_SND(...)            MZ_PRIV_SND_AUX(__VA_ARGS__, ~)
#define MZ_PRIV_SND_AUX(_x, y, ...) y

// Fixed-width integers
// /////////////////////////////////////////////////////////////////////////////

typedef uint8_t mz_prim_U8;
typedef uint16_t mz_prim_U16;
typedef uint32_t mz_prim_U32;
typedef uint64_t mz_prim_U64;
typedef unsigned __int128 mz_prim_U128;

typedef int8_t mz_prim_I8;
typedef int16_t mz_prim_I16;
typedef int32_t mz_prim_I32;
typedef int64_t mz_prim_I64;
typedef __int128 mz_prim_I128;

#define MZ_U8_MIN   UINT8_C(0)
#define MZ_U8_MAX   UINT8_MAX
#define MZ_U16_MIN  UINT16_C(0)
#define MZ_U16_MAX  UINT16_MAX
#define MZ_U32_MIN  UINT32_C(0)
#define MZ_U32_MAX  UINT32_MAX
#define MZ_U64_MIN  UINT64_C(0)
#define MZ_U64_MAX  UINT64_MAX
#define MZ_U128_MIN ((unsigned __int128)0)
#define MZ_U128_MAX (~MZ_U128_MIN)

#define MZ_I8_MIN   INT8_MIN
#define MZ_I8_MAX   INT8_MAX
#define MZ_I16_MIN  INT16_MIN
#define MZ_I16_MAX  INT16_MAX
#define MZ_I32_MIN  INT32_MIN
#define MZ_I32_MAX  INT32_MAX
#define MZ_I64_MIN  INT64_MIN
#define MZ_I64_MAX  INT64_MAX
#define MZ_I128_MIN (~MZ_I128_MAX)
#define MZ_I128_MAX ((__int128)(MZ_U128_MAX >> 1))

// The minimum size of a `char` array that is able to contain an arbitrary
// integer in decimal notation. It is equal to the number of characters in the
// minimum 128-bit signed number: `-170141183460469231731687303715884105728`,
// plus the null character.
#define MZ_MAX_INT_PRINT_SIZE (40 + /* the null character */ 1)

// Macro routines for fixed-width integers
// /////////////////////////////////////////////////////////////////////////////

#define MZ_ENUM_INT_TYPES(X, x)                                                \
    X(U, 8, x)                                                                 \
    X(U, 16, x)                                                                \
    X(U, 32, x)                                                                \
    X(U, 64, x)                                                                \
    X(I, 8, x)                                                                 \
    X(I, 16, x)                                                                \
    X(I, 32, x)                                                                \
    X(I, 64, x)

#define MZ_ENUM_INT_TYPES_EXTENDED(X, x)                                       \
    MZ_ENUM_INT_TYPES(X, x)                                                    \
    X(U, 128, x)                                                               \
    X(I, 128, x)

// For using from within `MZ_ENUM_INT_TYPES` and `MZ_ENUM_INT_TYPES_EXTENDED`.
// Otherwise, the macros will get blocked.
#define MZ_ENUM_INT_TYPES_2(X, x)                                              \
    X(U, 8, x)                                                                 \
    X(U, 16, x)                                                                \
    X(U, 32, x)                                                                \
    X(U, 64, x)                                                                \
    X(I, 8, x)                                                                 \
    X(I, 16, x)                                                                \
    X(I, 32, x)                                                                \
    X(I, 64, x)

#define MZ_ENUM_INT_TYPES_EXTENDED_2(X, x)                                     \
    MZ_ENUM_INT_TYPES_2(X, x)                                                  \
    X(U, 128, x)                                                               \
    X(I, 128, x)

#define MZ_ENUM_INT_OP2(X, x)                                                  \
    X(add, x)                                                                  \
    X(sub, x)                                                                  \
    X(mul, x)                                                                  \
    X(div, x)                                                                  \
    X(rem, x)                                                                  \
    X(bit_or, x)                                                               \
    X(bit_and, x)                                                              \
    X(bit_xor, x)                                                              \
    X(shift_left, x)                                                           \
    X(shift_right, x)                                                          \
    X(equal, x)                                                                \
    X(not_equal, x)                                                            \
    X(greater_than, x)                                                         \
    X(greater_than_equal, x)                                                   \
    X(less_than, x)                                                            \
    X(less_than_equal, x)

#define MZ_PRIV_IS_SIGNED(signedness) MZ_PRIV_IS_SIGNED_##signedness
#define MZ_PRIV_IS_SIGNED_U           false
#define MZ_PRIV_IS_SIGNED_I           true

#define MZ_PRIV_INT_FMT(signedness, bitness)                                   \
    MZ_PRIV_INT_FMT_##signedness(bitness)
#define MZ_PRIV_INT_FMT_U(bitness) PRIu##bitness
#define MZ_PRIV_INT_FMT_I(bitness) PRId##bitness

#define MZ_PRIV_INT_FMT_TY(signedness, bitness)                                \
    MZ_PRIV_INT_FMT_TY_##signedness(bitness)
#define MZ_PRIV_INT_FMT_TY_U(bitness) "u" #bitness
#define MZ_PRIV_INT_FMT_TY_I(bitness) "i" #bitness

#define MZ_PRIV_INT_REPR(signedness, bitness)                                  \
    MZ_PRIV_SND(                                                               \
        MZ_PRIV_INT_REPR_##bitness(signedness), mz_prim_##signedness##bitness)
#define MZ_PRIV_INT_REPR_128(signedness)                                       \
    /* 128-bit integers are boxed to fit values in two machine words. */       \
    ~, const mz_prim_##signedness##128 *

// Garbage collection utilities
// /////////////////////////////////////////////////////////////////////////////

#define X(name, gc)                                                            \
    inline static void *name(const size_t nbytes) {                            \
        assert(nbytes > 0);                                                    \
                                                                               \
        void *mem = gc(nbytes);                                                \
        if (NULL == mem) {                                                     \
            fprintf(stderr, "Failed to allocate %zu bytes\n", nbytes);         \
            abort();                                                           \
        }                                                                      \
                                                                               \
        return mem;                                                            \
    }

X(mz_malloc, GC_MALLOC)
X(mz_malloc_atomic, GC_MALLOC_ATOMIC)

#undef X

#define X(signedness, bitness)                                                 \
    inline static const mz_prim_##signedness##bitness *                        \
    mz_priv_box_##signedness##bitness(const mz_prim_##signedness##bitness x) { \
        mz_prim_##signedness##bitness *const y = mz_malloc_atomic(sizeof(*y)); \
        *y = x;                                                                \
        return y;                                                              \
    }

X(U, 128)
X(I, 128)

#undef X

// 128-bit integers utilities
// /////////////////////////////////////////////////////////////////////////////

inline static char *
mz_priv_sprint_u128(const unsigned __int128 n, char *restrict buffer) {
    if (n >= 10) {
        buffer = mz_priv_sprint_u128(n / 10, buffer);
    }

    *(buffer++) = "0123456789"[n % 10];
    return buffer;
}

inline static char *
mz_priv_sprint_i128(const __int128 n, char *restrict buffer) {
    if (n < 0) {
        *(buffer++) = '-';
    }

    unsigned __int128 unsigned_n = 0;
    if (MZ_I128_MIN == n) {
        // `MZ_I128_MIN` is irrepresentable as a positive integer.
        unsigned_n = (unsigned __int128)MZ_I128_MAX + 1;
    } else if (n < 0) {
        unsigned_n = (unsigned __int128)-n;
    } else {
        unsigned_n = (unsigned __int128)n;
    }

    return mz_priv_sprint_u128(unsigned_n, buffer);
}

// Language values
// /////////////////////////////////////////////////////////////////////////////

typedef mz_Value *const mz_EnvPtr, *const mz_ArgsPtr;

typedef const struct mz_thunk {
    mz_Value (*callback)(mz_EnvPtr env);
    mz_Value *env;
} mz_Thunk;

#define ENUM_VALUE_VARIANTS                                                    \
    MZ_ENUM_INT_TYPES_EXTENDED(X_INT, ~)                                       \
    X(String, sds)                                                             \
    X(Thunk, mz_Thunk *)

#define X_INT(signedness, bitness, _dummy)                                     \
    X(signedness##bitness, MZ_PRIV_INT_REPR(signedness, bitness))

#define X(variant, _T) mz_##variant##Tag,

// clang-format off
enum {
    ENUM_VALUE_VARIANTS
    op_T, op_F
}; // clang-format on

#undef X

// The value representation is a 16-byte "fat pointer": the first 64 bits are
// for the tag, the last 64 bits are for the data. There are a lot of possible
// representations (see [1]); our choice has a merit of simplicity and the
// ability to have unboxed data of 64 bits and less. The single tag field is
// used both for built-in types (such as integers/strings) and user-defined
// constructors (such as `Nil` or `Cons`).
//
// [1] Gudeman, David. Representing type information in dynamically typed
// languages. Univ., 1995.
struct mz_value {
    uint64_t tag;

#define X(variant, T) T variant;

    // clang-format off
    union {
        ENUM_VALUE_VARIANTS
        struct mz_value *payload;
    }; // clang-format on

#undef X
};

static_assert(
    sizeof(mz_Value) == 16, "The value representation must be two words long");

#undef ENUM_VALUE_VARIANTS
#undef X_INT

#define MZ_GET_UNCHECKED(variant, ...)                                         \
    MZ_PRIV_SND(MZ_GET_UNCHECKED_##variant, MZ_PRIV_ID)((__VA_ARGS__).variant)
#define MZ_GET_UNCHECKED_U128 ~, MZ_PRIV_DEREF
#define MZ_GET_UNCHECKED_I128 ~, MZ_PRIV_DEREF

#ifdef NDEBUG

#define MZ_GET MZ_GET_UNCHECKED

#define MZ_UNEXPECTED_TAG(_tag) __builtin_unreachable()

#else

#define MZ_GET(variant, ...)                                                   \
    MZ_PRIV_GET_SYM(MZ_PRIV_GEN_SYM(MZ_GET_, val), variant, __VA_ARGS__)
#define MZ_PRIV_GET_SYM(sym, variant, ...)                                     \
    ({                                                                         \
        mz_Value sym = (__VA_ARGS__);                                          \
        if (mz_##variant##Tag != sym.tag) {                                    \
            mz_stuck(sym.tag, MZ_TAG_MISMATCH(variant));                       \
        }                                                                      \
        MZ_GET_UNCHECKED(variant, sym);                                        \
    })

#define MZ_UNEXPECTED_TAG(tag)                                                 \
    (fprintf(stderr, "An unexpected tag: %" PRIu64 "\n", tag), abort())

#endif // NDEBUG

// Value forcing takes place whenever a variable coming from a constructor
// pattern is used somewhere in the code.
inline static mz_Value mz_force(mz_Value v) {
    mz_Thunk *const thunk = MZ_GET(Thunk, v);

    return thunk->callback(thunk->env);
}

// Value constructors
// /////////////////////////////////////////////////////////////////////////////

#define MZ_VALUE(variant, ...)                                                 \
    ((mz_Value){mz_##variant##Tag, .variant = __VA_ARGS__})

#define MZ_INT(signedness, bitness, x) mz_priv_##signedness##bitness(x)
#define MZ_INT_EXPLICIT_CAST(signedness, bitness, x)                           \
    MZ_INT(signedness, bitness, (mz_prim_##signedness##bitness)x)

#define X(signedness, bitness, _dummy)                                         \
    inline static mz_Value mz_priv_##signedness##bitness(                      \
        const mz_prim_##signedness##bitness x) {                               \
        return MZ_VALUE(                                                       \
            signedness##bitness,                                               \
            MZ_PRIV_SND(X_##bitness(signedness), MZ_PRIV_ID)(x));              \
    }
#define X_128(signedness) ~, mz_priv_box_##signedness##128

MZ_ENUM_INT_TYPES_EXTENDED(X, ~)

#undef X
#undef X_128

#define MZ_STRING(s) MZ_VALUE(String, sdsnewlen(s, MZ_PRIV_STRING_SIZE(s)))

// GCC does not allow arbitrary 128-bit literals, so we have to construct them
// from the high and low 64-bit parts.
#define X(signedness)                                                          \
    inline static mz_prim_##signedness##128 mz_##signedness##128_of_parts(     \
        const mz_prim_U64 high, const mz_prim_U64 low) {                       \
        return (mz_prim_##signedness##128)high << 64 |                         \
               (mz_prim_##signedness##128)low;                                 \
    }

X(U)
X(I)

#undef X

#define MZ_DATA(tag, nvalues, ...)                                             \
    ((mz_Value){tag,                                                           \
                .payload = MZ_PRIV_BOX_MANY(mz_Value, nvalues, __VA_ARGS__)})
#define MZ_EMPTY_DATA(tag) ((mz_Value){tag, .payload = NULL})
#define MZ_BOOL(b)         ((b) ? MZ_EMPTY_DATA(op_T) : MZ_EMPTY_DATA(op_F))

#define MZ_THUNK(callback, nvalues, ...)                                       \
    MZ_VALUE(                                                                  \
        Thunk,                                                                 \
        MZ_PRIV_BOX(                                                           \
            mz_Thunk,                                                          \
            {callback, MZ_PRIV_BOX_MANY(mz_Value, nvalues, __VA_ARGS__)}))
#define MZ_EMPTY_THUNK(callback)                                               \
    MZ_VALUE(Thunk, MZ_PRIV_BOX(mz_Thunk, {callback, NULL}))
#define MZ_SIMPLE_THUNK(x)      MZ_THUNK(mz_priv_env_var, 1, x)
#define MZ_SIMPLE_THUNK_LAZY(x) MZ_THUNK(mz_priv_env_var_lazy, 1, x)

// Here `inline` is used to suppress the unused function warning.
inline static mz_Value mz_priv_env_var(mz_EnvPtr env) { return env[0]; }

inline static mz_Value mz_priv_env_var_lazy(mz_EnvPtr env) {
    return mz_force(env[0]);
}

// Unary operators
// /////////////////////////////////////////////////////////////////////////////

inline static mz_Value mz_string_of(mz_Value v);

inline static mz_Value mz_panic(mz_Value v) {
    switch (v.tag) {
#define X(signedness, bitness, _dummy)                                         \
    case mz_##signedness##bitness##Tag:                                        \
        fprintf(                                                               \
            stderr, "Execution panic: %s%s\n", mz_string_of(v).String,         \
            MZ_PRIV_INT_FMT_TY(signedness, bitness));                          \
        break;

        MZ_ENUM_INT_TYPES_EXTENDED(X, ~)

#undef X

    case mz_StringTag:;
        const sds s = sdscatrepr(sdsempty(), v.String, sdslen(v.String));
        fprintf(stderr, "Execution panic: %s\n", s);
        break;

    case mz_ThunkTag:
        fprintf(stderr, "Execution panic: <opaque thunk>\n");
        break;

    default:
        fprintf(
            stderr, "Execution panic: <opaque data> (tag %" PRIu64 ")\n",
            v.tag);
        break;
    }

    abort();
}

#define X(signedness, bitness, _dummy)                                         \
    inline static mz_Value mz_##signedness##bitness##_cast(mz_Value v) {       \
        typedef mz_prim_##signedness##bitness TargetTy;                        \
        TargetTy y = 0;                                                        \
                                                                               \
        /* Fix some constants for `X_CASE`. Constant propagation & folding     \
         * should optimize these variables away. */                            \
        static const TargetTy target_max = MZ_##signedness##bitness##_MAX;     \
        static const bool is_signed_target = MZ_PRIV_IS_SIGNED(signedness);    \
        static const int target_bitness = bitness;                             \
                                                                               \
        MZ_ENUM_INT_TYPES_EXTENDED_2(X_CASE, ~)                                \
        MZ_UNEXPECTED_TAG(v.tag);                                              \
                                                                               \
    done:                                                                      \
        return MZ_INT(signedness, bitness, y);                                 \
    }

#define X_CASE(signedness, bitness, _dummy)                                    \
    if (mz_##signedness##bitness##Tag == v.tag) {                              \
        typedef mz_prim_##signedness##bitness SourceTy;                        \
        const SourceTy x = MZ_GET_UNCHECKED(signedness##bitness, v);           \
        y = (TargetTy)x;                                                       \
        if ((SourceTy)y != x ||                                                \
            (MZ_PRIV_IS_SIGNED(signedness) && !is_signed_target && x < 0) ||   \
            (!MZ_PRIV_IS_SIGNED(signedness) && is_signed_target &&             \
             target_bitness <= bitness && x > (SourceTy)target_max)) {         \
            return mz_panic(MZ_STRING("out of range"));                        \
        }                                                                      \
        goto done;                                                             \
    }

MZ_ENUM_INT_TYPES_EXTENDED(X, ~)

#undef X
#undef X_CASE

inline static mz_Value mz_bit_not(mz_Value v) {
    switch (v.tag) {
#define X(signedness, bitness, _dummy)                                         \
    case mz_##signedness##bitness##Tag:                                        \
        return MZ_INT(                                                         \
            signedness, bitness, ~MZ_GET_UNCHECKED(signedness##bitness, v));

        MZ_ENUM_INT_TYPES_EXTENDED(X, ~)

#undef X

    default:
        MZ_UNEXPECTED_TAG(v.tag);
    }
}

inline static mz_Value mz_string_of(mz_Value v) {
    char buffer[MZ_MAX_INT_PRINT_SIZE] = {0};

    switch (v.tag) {
#define X(signedness, bitness, _dummy)                                         \
    case mz_##signedness##bitness##Tag:                                        \
        sprintf(                                                               \
            buffer, "%" MZ_PRIV_INT_FMT(signedness, bitness),                  \
            MZ_GET_UNCHECKED(signedness##bitness, v));                         \
        break;

        MZ_ENUM_INT_TYPES(X, ~)

#undef X

    case mz_U128Tag:
        mz_priv_sprint_u128(*v.U128, buffer);
        break;
    case mz_I128Tag:
        mz_priv_sprint_i128(*v.I128, buffer);
        break;
    case mz_StringTag:
        return v;

    default:
        MZ_UNEXPECTED_TAG(v.tag);
    }

    return MZ_VALUE(String, sdsnew(buffer));
}

inline static mz_Value mz_string_of_char(mz_Value v) {
    const char x = (char)MZ_GET(U8, v);

    return MZ_VALUE(String, sdsnewlen(&x, 1));
}

inline static mz_Value mz_length_of(mz_Value v) {
    const sds s = MZ_GET(String, v);

    return MZ_INT_EXPLICIT_CAST(U, 64, sdslen(s));
}

// Binary operators
// /////////////////////////////////////////////////////////////////////////////

// The order of evaluation of arguments is unspecified by C; this macro
// preserves the left-to-right order of Mazeppa.
#define MZ_OP2(t1, op, t2)                                                     \
    MZ_PRIV_OP2_SYM(MZ_PRIV_GEN_SYM(MZ_OP2_, seq_point), t1, op, t2)
#define MZ_PRIV_OP2_SYM(sym, t1, op, t2)                                       \
    ({                                                                         \
        /* Force a sequence point for `t1`. */                                 \
        mz_Value sym = t1;                                                     \
        mz_priv_##op(sym, t2);                                                 \
    })

#define X(signedness, bitness, _dummy)                                         \
    X_OP(add, __builtin_add_overflow, signedness, bitness)                     \
    X_OP(sub, __builtin_sub_overflow, signedness, bitness)                     \
    X_OP(mul, __builtin_mul_overflow, signedness, bitness)

#define X_OP(op, builtin_f, signedness, bitness)                               \
    inline static mz_Value mz_priv_##signedness##bitness##_##op(               \
        mz_prim_##signedness##bitness x, mz_Value v2) {                        \
        const mz_prim_##signedness##bitness y =                                \
            MZ_GET(signedness##bitness, v2);                                   \
                                                                               \
        mz_prim_##signedness##bitness z = 0;                                   \
        if (builtin_f(x, y, &z)) {                                             \
            return mz_panic(MZ_STRING("out of range"));                        \
        } else {                                                               \
            return MZ_INT(signedness, bitness, z);                             \
        }                                                                      \
    }

MZ_ENUM_INT_TYPES_EXTENDED(X, ~)

#undef X
#undef X_OP

#define X(signedness, bitness, _dummy)                                         \
    X_OP(div, signedness, bitness, x / y)                                      \
    X_OP(rem, signedness, bitness, x % y)

#define X_OP(op, signedness, bitness, ...)                                     \
    inline static mz_Value mz_priv_##signedness##bitness##_##op(               \
        mz_prim_##signedness##bitness x, mz_Value v2) {                        \
        const mz_prim_##signedness##bitness y =                                \
            MZ_GET(signedness##bitness, v2);                                   \
                                                                               \
        if (0 == y) {                                                          \
            /* Division by zero. */                                            \
            return mz_panic(MZ_STRING("out of range"));                        \
        } else if (X_CHECK_OVERFLOW_##signedness(bitness, x, y)) {             \
            /* Overflow. */                                                    \
            return mz_panic(MZ_STRING("out of range"));                        \
        } else {                                                               \
            return MZ_INT(signedness, bitness, (__VA_ARGS__));                 \
        }                                                                      \
    }
#define X_CHECK_OVERFLOW_U(_bitness, _x, _y) false
#define X_CHECK_OVERFLOW_I(bitness, x, y)    MZ_I##bitness##_MIN == x && -1 == y

MZ_ENUM_INT_TYPES_EXTENDED(X, ~)

#undef X
#undef X_OP
#undef X_CHECK_OVERFLOW_U
#undef X_CHECK_OVERFLOW_I

#define X(signedness, bitness, _dummy)                                         \
    X_OP(bit_or, signedness, bitness, MZ_INT(signedness, bitness, x | y))      \
    X_OP(bit_and, signedness, bitness, MZ_INT(signedness, bitness, (x & y)))   \
    X_OP(bit_xor, signedness, bitness, MZ_INT(signedness, bitness, x ^ y))     \
    X_OP(equal, signedness, bitness, MZ_BOOL(x == y))                          \
    X_OP(not_equal, signedness, bitness, MZ_BOOL(x != y))                      \
    X_OP(greater_than, signedness, bitness, MZ_BOOL(x > y))                    \
    X_OP(greater_than_equal, signedness, bitness, MZ_BOOL(x >= y))             \
    X_OP(less_than, signedness, bitness, MZ_BOOL(x < y))                       \
    X_OP(less_than_equal, signedness, bitness, MZ_BOOL(x <= y))

#define X_OP(op, signedness, bitness, ...)                                     \
    inline static mz_Value mz_priv_##signedness##bitness##_##op(               \
        mz_prim_##signedness##bitness x, mz_Value v2) {                        \
        const mz_prim_##signedness##bitness y =                                \
            MZ_GET(signedness##bitness, v2);                                   \
                                                                               \
        return __VA_ARGS__;                                                    \
    }

MZ_ENUM_INT_TYPES_EXTENDED(X, ~)

#undef X
#undef X_OP

#define X(signedness, bitness, _dummy)                                         \
    X_OP(shift_left, signedness, bitness, x << y)                              \
    X_OP(shift_right, signedness, bitness, x >> y)

#define X_OP(op, signedness, bitness, ...)                                     \
    inline static mz_Value mz_priv_##signedness##bitness##_##op(               \
        mz_prim_##signedness##bitness x, mz_Value v2) {                        \
        const mz_prim_##signedness##bitness y =                                \
            MZ_GET(signedness##bitness, v2);                                   \
                                                                               \
        if (y < 0) {                                                           \
            /* Underflow. */                                                   \
            return mz_panic(MZ_STRING("out of range"));                        \
        } else if (y >= bitness) {                                             \
            /* Overflow. */                                                    \
            return mz_panic(MZ_STRING("out of range"));                        \
        } else {                                                               \
            return MZ_INT_EXPLICIT_CAST(signedness, bitness, (__VA_ARGS__));   \
        }                                                                      \
    }

MZ_ENUM_INT_TYPES_EXTENDED(X, ~)

#undef X
#undef X_OP

#define X(op, _dummy)                                                          \
    inline static mz_Value mz_priv_##op(mz_Value v1, mz_Value v2) {            \
        switch (v1.tag) {                                                      \
            MZ_ENUM_INT_TYPES_EXTENDED(X_INT_CASE, op)                         \
            MZ_PRIV_SND(X_CASE_##op, /* empty */);                             \
                                                                               \
        default:                                                               \
            MZ_UNEXPECTED_TAG(v1.tag);                                         \
        }                                                                      \
    }

#define X_INT_CASE(signedness, bitness, op)                                    \
    case mz_##signedness##bitness##Tag:                                        \
        return mz_priv_##signedness##bitness##_##op(                           \
            MZ_GET_UNCHECKED(signedness##bitness, v1), v2);

#define X_STRING_CASE(cmp)                                                     \
    case mz_StringTag:                                                         \
        return MZ_BOOL(sdscmp(v1.String, MZ_GET(String, v2)) cmp 0)

#define X_CASE_equal              ~, X_STRING_CASE(==)
#define X_CASE_not_equal          ~, X_STRING_CASE(!=)
#define X_CASE_greater_than       ~, X_STRING_CASE(>)
#define X_CASE_greater_than_equal ~, X_STRING_CASE(>=)
#define X_CASE_less_than          ~, X_STRING_CASE(<)
#define X_CASE_less_than_equal    ~, X_STRING_CASE(<=)

MZ_ENUM_INT_OP2(X, ~)

#undef X
#undef X_INT_CASE
#undef X_STRING_CASE

#undef X_CASE_equal
#undef X_CASE_not_equal
#undef X_CASE_greater_than
#undef X_CASE_greater_than_equal
#undef X_CASE_less_than
#undef X_CASE_less_than_equal

inline static mz_Value mz_priv_plus_plus(mz_Value v1, mz_Value v2) {
    const sds s1 = MZ_GET(String, v1);
    const sds s2 = MZ_GET(String, v2);

    return MZ_VALUE(String, sdscatsds(sdsdup(s1), s2));
}

inline static mz_Value mz_priv_get(mz_Value v1, mz_Value v2) {
    const sds s = MZ_GET(String, v1);
    const uint64_t i = MZ_GET(U64, v2);

    if (i >= sdslen(s)) {
        return mz_panic(MZ_STRING("out of bounds"));
    }

    return MZ_INT_EXPLICIT_CAST(U, 8, s[i]);
}

// TODO: implement floating-point types through FFI. Floats can be represented
// as unsigned integers via type punning.
// TODO: implement modular arithmetic in a similar way.

#endif // MAZEPPA_H
