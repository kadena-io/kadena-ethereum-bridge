#include <openssl/evp.h>

/* *************************************************************************** */
/* Generic Tools */

#define CHECKED(f)            \
    if (! (f)) {              \
        ok = 0; goto finally; \
    }

/* *************************************************************************** */
/* Keccak-256 */

#if OPENSSL_VERSION_NUMBER >= 0x31000000L

#elif OPENSSL_VERSION_NUMBER >= 0x30000000L
typedef struct keccak1600_ctx_st KECCAK1600_CTX;
typedef KECCAK1600_CTX ETH_KECCAK256_CTX;
typedef KECCAK1600_CTX ETH_KECCAK512_CTX;

#elif OPENSSL_VERSION_NUMBER >= 0x10100000L
typedef EVP_MD_CTX ETH_KECCAK256_CTX;
typedef EVP_MD_CTX ETH_KECCAK512_CTX;

#endif

// KECCAK-256
ETH_KECCAK256_CTX *eth_keccak256_newctx();
int eth_keccak256_init(ETH_KECCAK256_CTX *ctx);
int eth_keccak256_reset(ETH_KECCAK256_CTX *ctx);
int eth_keccak256_update(ETH_KECCAK256_CTX *ctx, const void *p, size_t l);
int eth_keccak256_final(ETH_KECCAK256_CTX *ctx, unsigned char *md);
void eth_keccak256_freectx(ETH_KECCAK256_CTX *ctx);

// ETH_KECCAK-512
ETH_KECCAK512_CTX *eth_keccak512_newctx();
int eth_keccak512_init(ETH_KECCAK512_CTX *ctx);
int eth_keccak512_reset(ETH_KECCAK512_CTX *ctx);
int eth_keccak512_update(ETH_KECCAK512_CTX *ctx, const void *p, size_t l);
int eth_keccak512_final(ETH_KECCAK512_CTX *ctx, unsigned char *md);
void eth_keccak512_freectx(ETH_KECCAK512_CTX *ctx);

