# Basic mcash MAPI client in common lisp.

Depends on several other packages, most of them can be obtained through
Quicklisp. cl-rsasign is available at: https://github.com/mandus/cl-rsasign

In order to use the client you should create $HOME/.mcashapirc with content
like this:

```json
{
   "apiurl": "https://mcashtestbed.appspot.com/merchant/v1/",
   "qrurl": "https://mcashtestbed.appspot.com/shortlink/v1/qr_image/",
   "merchantid": "MAPI-id",
   "merchantuser": "MAPI-user",
   "merchantusersecret": "secret",
   "usetestbed": true,
   "testbedtoken": "the-token",
   "pemfile": "~/link/to/users/pemfile.pem",
   "debug": true
}
```

The pem-file is the private Mcash-pem.

I have only been able to run this on OS X thus far.
