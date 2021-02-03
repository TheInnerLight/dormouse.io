---
title: "Dormouse Client - Request/Response"
author: Me
date: Jan 31, 2021
tags: [haskell, dormouse]
description: Dormouse Client - Request/Response
isTop: False
---

## Simple Request/Response Model

The simplest possible conception of an http request in Haskell would be a function of the form `Request a -> IO (Response b)`.  In Dormouse, we seek to deviate as little as possible from that form and where we do, we do so only to support additional expressiveness and safety against constructing incorrect requests.


## Requests

Dormouse's representation of HTTP requests look like this:

```haskell
data HttpRequest url method body contentTag acceptTag = HttpRequest 
  { requestMethod :: !(HttpMethod method)
  , requestUrl :: !url
  , requestHeaders :: Map.Map HeaderName ByteString
  , requestBody :: body
  }
```

Hopefully there is nothing too surprising going on here, an HTTP request is an entity consisting of a request method, url, a collection of headers and a content body.  Let's take a look at each of the type parameters:

- The `url` type parameter tags the request with the Url scheme such that we can define functions that shoud accept strictly `http` or `https`; or either of the above.

- The `method` type parameter tags the request with the name of the HTTP verb, this is used, for example to define functions that will not type-check if you attempt to supply a request body to a request method that does not allow them, e.g. `"GET"`.  

- The `body` type parameter allows us to support arbitrary data in the content body of the request.

- The `contentTag` and `acceptTag` type parameters are a little more complicated, these are phantom types that are used to indicate [media types](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types) in the request `Content-Type` and `Accept` headers.  We will see how they are used later!

Let's also introduce some Dormouse defined tyes:

- `HeaderName` is simply a type alias for `CI ByteString` (a case-insensitive Strict ByteString).
- `HttpMethod method` is a GADT with all your standard HTTP Verbs (e.g. `GET :: HttpMethod "GET"`) defined as well as support for custom methods.

**Note:** Serialisation of the request is handled by a multi-parameter type class which considers the `body` of the response, i.e. the structure of the data, and the `contentTag` which describes the format the data should be serialised in.

---

### Request Building

Dormouse provides a series of functions that can be used to create and incrementally transform an HTTP request towards a finalised form.  Below, we describe a suggested path for convenient construction of

#### Step one: request method and URL

Dormouse defines some convenient helper functions for creating request templates for each HTTP Verb against a supplied Url.

Here is the one for post:  

```haskell  
post :: IsUrl url => url -> HttpRequest url "POST" Empty EmptyPayload acceptTag   
```  
  
Example use:

```haskell
r :: HttpRequest (Url "https") "POST" Empty EmptyPayload acceptTag
r = post [https|https://postman-echo.com/post|]
```

This generates a basic HTTP POST request with no payload.

#### Step two: request body

If we are using an HTTP method like `POST` that permits request bodies, we can supply a request body using the `supplyBody` function.

```haskell
supplyBody :: (AllowedBody method b, RequestPayload b contentTag) 
           => Proxy contentTag 
           -> b 
           -> HttpRequest url method b' contentTag' acceptTag 
           -> HttpRequest url method b contentTag acceptTag
```

The supply function transforms the initial request in to ways:

1. It adds the supplied request body to the request.
2. It adds a `Content-Type` header to the request based on the supplied `contentTag`.

Example use:

```haskell
r' :: HttpRequest (Url "https") "POST" String JsonPayload acceptTag
r' = supplyBody json ("Test" :: String) $ post [https|https://postman-echo.com/post|]
```

In this example, a `Content-Type: application/json` header would be added to the request.

#### Step three: supply an accept header

We can supply an `Accept` header in our request using the `accept` function.

```haskell
accept :: HasMediaType acceptTag 
       => Proxy acceptTag 
       -> HttpRequest url method b contentTag acceptTag 
       -> HttpRequest url method b contentTag acceptTag
```

This function fixes the `acceptTag` phantom type and sets a corresponding `Accept` header.

While this function is attached to the request, it is primarily useful because it allows us to introduce expectations at the type level about what kind of response we expect to receive.

Example use:

```haskell
r'' :: HttpRequest (Url "https") "POST" String JsonPayload JsonPayload
r'' = accept json $ supplyBody json ("Test" :: String) $ post [https|https://postman-echo.com/post|]
```
---

## Responses

Dormouse's representation of HTTP responses look like this:

```haskell
data HttpResponse body = HttpResponse
  { responseStatusCode :: !Int
  , responseHeaders :: Map.Map HeaderName SB.ByteString
  , responseBody :: body
  }
```

I think this is mostly self explanatory but the `body` type parameter allows us to support arbitrary data in the content body of the response.

Getting an HTTP response in Dormouse is most often achieved by the `expect` function.  Let's take a look at its type signature

```haskell
expect :: (MonadDormouseClient m, RequestPayload b contentTag, ResponsePayload b' acceptTag, IsUrl url)
       => HttpRequest url method b contentTag acceptTag -> m (HttpResponse b')
```
This function does several things:

 - Serialises the arbitrary data in the HTTP request's content body into a stream of `Word8`s.
 - Delegates sending the HTTP request, including the content body stream of `Word8`s to some `MonadDormouseClient m`
 - Receives the HTTP response, including a content body stream of `Word8`s.
 - Deserialises the HTTP responses' content body stream of `Word8`s into some expected type `b'`.

