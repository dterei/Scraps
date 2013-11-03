// the memcached item (key-value) representation and storage.
#include "items.h"

#include <unistd.h>

// lookup a key-value.
item *item_get(const char *key, const size_t nkey) {
	return NULL;
}

// decrease the ref count on the item and add to free-list if 0.
void item_remove(item *item) {
}

// update an items position in the LRU.
void item_update(item *item) {
}

