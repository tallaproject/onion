

# Module onion_authenticate_cell #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Onion Authenticate Cell Utilities.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="types"></a>

## Data Types ##




### <a name="type-config">config()</a> ###


<pre><code>
config() = #{client_identity_public_key =&gt; <a href="onion_rsa.md#type-public_key">onion_rsa:public_key()</a>, server_identity_public_key =&gt; <a href="onion_rsa.md#type-public_key">onion_rsa:public_key()</a>, server_log =&gt; binary(), client_log =&gt; binary(), server_certificate =&gt; <a href="public_key.md#type-der_encoded">public_key:der_encoded()</a>, ssl_session =&gt; <a href="onion_ssl_session.md#type-t">onion_ssl_session:t()</a>, authentication_secret_key =&gt; <a href="onion_rsa.md#type-secret_key">onion_rsa:secret_key()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create-1">create/1</a></td><td>Create an authenticate cell from the given configuration.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="create-1"></a>

### create/1 ###

<pre><code>
create(Config) -&gt; <a href="onion_cell.md#type-t">onion_cell:t()</a>
</code></pre>

<ul class="definitions"><li><code>Config = <a href="#type-config">config()</a></code></li></ul>

Create an authenticate cell from the given configuration.

