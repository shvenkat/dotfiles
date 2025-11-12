# Guidelines for HTML, CSS and JS

## Sane defaults

    <!DOCTYPE html>
    <html lang="en-US">
    <head>
      <meta charset="utf-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <link rel="icon" href="/favicon.ico" type="image/x-icon">

## Security best practices

* Call only routes you control
* Use an auto-escaping templating engine
* Serve user-generated content only within HTML tags
* Set cookies with Secure, HttpOnly and SameSite=Lax
