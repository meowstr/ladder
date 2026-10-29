(module com *
  (import scheme)
  (import chicken.base)
  (import chicken.foreign)

  #>
  int com_open( const char * name );
  int com_write( const unsigned char * data, int len );
  int com_read( unsigned char * data, int len );
  int com_read_byte();
  void com_close();
  <#

  (define com-open (foreign-lambda int "com_open" c-string))
  (define com-close (foreign-lambda void "com_close"))

  (define com-write (foreign-lambda int "com_write" nonnull-bytevector int))
  (define com-read (foreign-lambda int "com_read" nonnull-bytevector int))
  (define com-read-byte (foreign-lambda int "com_read_byte" ))
  )
