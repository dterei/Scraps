#include <chrono>
#include <exception>
#include <iostream>
#include <memory>
#include <thread>

#include <condition_variable>
#include <mutex>

/* We hide the inner implementation of channel that shouldn't be moved */
namespace internal {

  /* A channel provides a buffered communication pipe. We only supported buffered
   * channels for now, not synchronous ones.
   */ 
  template <typename T>
  class Channel_
  {
  public:
    class closed_error : public std::exception
    {
      const char * what( void ) const noexcept override
      {
        return "Channel closed!";
      }
    };

  private:
    std::mutex mtx_;
    std::condition_variable cv_;
    std::unique_ptr<T[]> slots_;
    size_t size_;
    size_t used_;
    size_t wptr_;
    size_t rptr_;
    bool closed_;

  public:

    Channel_( size_t buf = 1 )
      : slots_{new T[buf]}
      , size_{buf}
      , used_{0}
      , wptr_{0}
      , rptr_{0}
      , mtx_{}
      , cv_{}
      , closed_{false}
    {}

    /* No move or copy */
    Channel_( const Channel_ & c ) = delete;
    Channel_ & operator=( const Channel_ & c ) = delete;
    Channel_( Channel_ && c ) = delete;
    Channel_ & operator=( Channel_ && c ) = delete;

    inline void close()
    {
      std::unique_lock<std::mutex> lck( mtx_ );
      closed_ = true;
      cv_.notify_all();
    }

    template <typename U>
    inline void send( U && u )
    {
      std::unique_lock<std::mutex> lck( mtx_ );
      if ( closed_ ) {
        throw closed_error();
      }

      if ( used_ == size_ ) {
        cv_.wait( lck );
        if ( closed_ ) {
          throw closed_error();
        }
      }
      cv_.notify_one();

      used_++;
      slots_[wptr_] = std::forward<U>( u );
      wptr_ = (wptr_ + 1) % size_;
    }

    inline T recv( void )
    {
      std::unique_lock<std::mutex> lck( mtx_ );
      if ( closed_ ) {
        throw closed_error();
      }

      if ( used_ == 0 ) {
        cv_.wait( lck );
        if ( closed_ ) {
          throw closed_error();
        }
      }
      cv_.notify_one();
      
      used_--;
      T t = slots_[rptr_];
      rptr_ = (rptr_ + 1) % size_;
      return t;
    }
  };
};

template<typename T>
class Channel
{
private:
  std::shared_ptr<internal::Channel_<T>> chn;

public:
    using closed_error = typename internal::Channel_<T>::closed_error;

    Channel( size_t buf = 1 ) : chn{new internal::Channel_<T>{buf}} {}

    inline void close() { chn->close(); }
    inline void send( T && t ) { chn->send( std::move( t ) ); }
    inline void send( const T & t ) { chn->send( t ); }
    inline T recv( void ) { return chn->recv(); }
};

using namespace std;

mutex cerr_mtx;

void runner( int id, Channel<int> chn, Channel<bool> done )
{
  try {
    for ( int i = 0; i < 10; i++ ) {
      chn.send( id );
      this_thread::sleep_for(chrono::milliseconds(10));
    }
  } catch ( Channel<int>::closed_error & e) {
    lock_guard<mutex> lck( cerr_mtx );
    cerr << "Channel closed: " << id << endl;
  }
  done.send( true );
}

constexpr size_t nthreads = 5;

int main( void )
{
  Channel<int> chn;
  Channel<bool> done;
  
  for ( int i = 0; i < nthreads; i++ ) {
    thread t( runner, i, ref( chn ), ref( done ) );
    t.detach();
  }

  int n = 0;
  do {
    int x = chn.recv();
    cout << "Received: " << x << endl;
    n++;
    if ( n > 10 ) {
      chn.close();
      break;
    }
  } while ( true );

  for ( int i = 0; i < nthreads; i++ ) {
    done.recv();
  }

  return 0;
}
