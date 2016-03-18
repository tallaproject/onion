

# Module onion_ssl #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

SSL API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="description"></a>

## Description ##
Utility functions to be used together with OTP's SSL application.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#client_random-1">client_random/1</a></td><td>Get the SSL client random value.</td></tr><tr><td valign="top"><a href="#master_secret-1">master_secret/1</a></td><td>Get the SSL master secret value.</td></tr><tr><td valign="top"><a href="#server_random-1">server_random/1</a></td><td>Get the SSL server random value.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="client_random-1"></a>

### client_random/1 ###

<pre><code>
client_random(Socket) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>Socket = <a href="ssl.md#type-sslsocket">ssl:sslsocket()</a></code></li></ul>

Get the SSL client random value.

<a name="master_secret-1"></a>

### master_secret/1 ###

<pre><code>
master_secret(Socket) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>Socket = <a href="ssl.md#type-sslsocket">ssl:sslsocket()</a></code></li></ul>

Get the SSL master secret value.

<a name="server_random-1"></a>

### server_random/1 ###

<pre><code>
server_random(Socket) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>Socket = <a href="ssl.md#type-sslsocket">ssl:sslsocket()</a></code></li></ul>

Get the SSL server random value.

