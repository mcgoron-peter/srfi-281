#| Some of this test data comes from RFC 4648.

   Copyright (c) 2000-2006 Simon Josefsson

   Regarding the abstract and sections 1, 3, 8, 10, 12, 13, and 14 of
   this document, that were written by Simon Josefsson ("the author",
   for the remainder of this section), the author makes no guarantees
   and is not responsible for any damage resulting from its use.  The
   author grants irrevocable permission to anyone to use, modify, and
   distribute it in any way that does not diminish the rights of anyone
   else to use, modify, and distribute it, provided that redistributed
   derivative works do not contain misleading author or version
   information and do not falsely purport to be IETF RFC documents.
   Derivative works need not be licensed under similar terms.
|#

(define (test-bytevector->base64)
  (test-group "bytevector->base64"
    (test-equal "" (bytevector->base64 #u8()))
    (test-equal "Zg==" (bytevector->base64 #u8(102)))
    (test-equal "Zm8=" (bytevector->base64 #u8(102 111)))
    (test-equal "Zm9v" (bytevector->base64 #u8(102 111 111)))
    (test-equal "Zm9vYg==" (bytevector->base64 #u8(102 111 111 98)))
    (test-equal "Zm9vYmE=" (bytevector->base64 #u8(102 111 111 98 97)))
    (test-equal "Zm9vYmFy" (bytevector->base64 #u8(102 111 111 98 97 114)))
    (test-equal "+/++" (bytevector->base64
                        #u8(#b11111011 #b11111111 #b10111110)))
    (test-equal "-_--" (bytevector->base64
                        #u8(#b11111011 #b11111111 #b10111110)
                        "-_"))))
