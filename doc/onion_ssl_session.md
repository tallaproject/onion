

# Module onion_ssl_session #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

SSL Session API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="types"></a>

## Data Types ##




### <a name="type-t">t()</a> ###


<pre><code>
t() = #{atom() =&gt; binary()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#certificate-1">certificate/1</a></td><td>Get the DER encoded certificate.</td></tr><tr><td valign="top"><a href="#cipher_suite-1">cipher_suite/1</a></td><td>Get the list of SSL ciphers.</td></tr><tr><td valign="top"><a href="#client_random-1">client_random/1</a></td><td>Get the client random value.</td></tr><tr><td valign="top"><a href="#from_client_socket-1">from_client_socket/1</a></td><td>Creatte an SSL session instance from a given client socket.</td></tr><tr><td valign="top"><a href="#from_server_socket-2">from_server_socket/2</a></td><td>Create an SSL session instance from a given server socket and certificate.</td></tr><tr><td valign="top"><a href="#master_secret-1">master_secret/1</a></td><td>Get the master secret value.</td></tr><tr><td valign="top"><a href="#protocol-1">protocol/1</a></td><td>Get the SSL protocol.</td></tr><tr><td valign="top"><a href="#server_random-1">server_random/1</a></td><td>Get the server random value.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="certificate-1"></a>

### certificate/1 ###

<pre><code>
certificate(T) -&gt; <a href="public_key.md#type-der_encoded">public_key:der_encoded()</a>
</code></pre>

<ul class="definitions"><li><code>T = <a href="#type-t">t()</a></code></li></ul>

Get the DER encoded certificate.

<a name="cipher_suite-1"></a>

### cipher_suite/1 ###

<pre><code>
cipher_suite(T) -&gt; <a href="ssl.md#type-ciphers">ssl:ciphers()</a>
</code></pre>

<ul class="definitions"><li><code>T = <a href="#type-t">t()</a></code></li></ul>

Get the list of SSL ciphers.

<a name="client_random-1"></a>

### client_random/1 ###

<pre><code>
client_random(T) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>T = <a href="#type-t">t()</a></code></li></ul>

Get the client random value.

<a name="from_client_socket-1"></a>

### from_client_socket/1 ###

<pre><code>
from_client_socket(Socket) -&gt; <a href="#type-t">t()</a>
</code></pre>

<ul class="definitions"><li><code>Socket = <a href="ssl.md#type-socket">ssl:socket()</a></code></li></ul>

Creatte an SSL session instance from a given client socket.

<a name="from_server_socket-2"></a>

### from_server_socket/2 ###

<pre><code>
from_server_socket(Socket, Certificate) -&gt; <a href="#type-t">t()</a>
</code></pre>

<ul class="definitions"><li><code>Socket = <a href="ssl.md#type-socket">ssl:socket()</a></code></li><li><code>Certificate = <a href="public_key.md#type-der_encoded">public_key:der_encoded()</a></code></li></ul>

Create an SSL session instance from a given server socket and certificate.

<a name="master_secret-1"></a>

### master_secret/1 ###

<pre><code>
master_secret(T) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>T = <a href="#type-t">t()</a></code></li></ul>

Get the master secret value.

<a name="protocol-1"></a>

### protocol/1 ###

<pre><code>
protocol(T) -&gt; <a href="ssl.md#type-protocol">ssl:protocol()</a>
</code></pre>

<ul class="definitions"><li><code>T = <a href="#type-t">t()</a></code></li></ul>

Get the SSL protocol.

<a name="server_random-1"></a>

### server_random/1 ###

<pre><code>
server_random(T) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>T = <a href="#type-t">t()</a></code></li></ul>

Get the server random value.

