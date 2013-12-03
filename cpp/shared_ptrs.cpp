#include <cstdio>
#include <memory>

int main(void)
{
  void *ptr;
	// allocate memory with a traditional pointer to it.
	int *raw_ptr = new int(10);
	// setup a 'smart' pointer to it.
	// a 'smart' pointer is basically a 'proxy' pointer, or another level of
	// indirection which we use to implement reference counting of the memory.
	// So we should avoid using the raw pointer or creating a second 'smart'
	// pointer to the same memroy through the raw pointer.
	std::shared_ptr<int> ref_ptr(raw_ptr);
	std::shared_ptr<int> p2;

  ptr = p2;

	// XXX: Shouldn't do this.
	std::shared_ptr<int> xxx_ref(raw_ptr);

	// observer smart poiner. (ref count = 1).
	printf("raw ptr (%p) *= %d\n", raw_ptr, *raw_ptr);
	printf("shared ptr (%p) *= %d\n", &ref_ptr, *ref_ptr);
	printf("shared ptr count = %ld\n", ref_ptr.use_count());
	printf("shared ptr get = %p\n", ref_ptr.get());

	// create another ref.
	p2 = ref_ptr;
	printf("raw ptr (%p) *= %d\n", raw_ptr, *raw_ptr);
	printf("shared ptr (%p) *= %d\n", &ref_ptr, *ref_ptr);
	printf("shared ptr count = %ld\n", ref_ptr.use_count());
	printf("shared ptr get = %p\n", ref_ptr.get());

	// kill the only ref (except our raw pointer).
	p2 = ref_ptr = NULL;
	printf("raw ptr (%p) *= %d\n", raw_ptr, *raw_ptr);
	printf("shared ptr [p2] (%p)\n", &p2);
	printf("shared ptr count = %ld\n", p2.use_count());
	printf("shared ptr get = %p\n", p2.get());

	// should result in double free...
	xxx_ref = NULL;
	printf("raw ptr (%p) *= %d\n", raw_ptr, *raw_ptr);
	printf("shared ptr (%p)\n", &xxx_ref);
	printf("shared ptr count = %ld\n", xxx_ref.use_count());
	printf("shared ptr get = %p\n", xxx_ref.get());

	return 0;
}

