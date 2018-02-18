

# Module onion_x509 #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

x509 API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="description"></a>

## Description ##

<a name="types"></a>

## Data Types ##




### <a name="type-options">options()</a> ###


<pre><code>
options() = #{public_key =&gt; <a href="onion_rsa.md#type-public_key">onion_rsa:public_key()</a>, valid_from =&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>, valid_to =&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>, subject =&gt; [{<a href="#type-subject">subject()</a>, term()}], issuer =&gt; [{<a href="#type-subject">subject()</a>, term()}], version =&gt; v1 | v2 | v3, serial =&gt; non_neg_integer(), digest =&gt; sha | sha256 | sha384 | sha512 | md5}
</code></pre>




### <a name="type-subject">subject()</a> ###


<pre><code>
subject() = name | email | city | state | org | org_unit | country | serial | title | dnQualifier
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create_certificate-1">create_certificate/1</a></td><td></td></tr><tr><td valign="top"><a href="#der_decode-1">der_decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#der_encode-1">der_encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_self_signed-1">is_self_signed/1</a></td><td></td></tr><tr><td valign="top"><a href="#not_after-1">not_after/1</a></td><td></td></tr><tr><td valign="top"><a href="#not_before-1">not_before/1</a></td><td></td></tr><tr><td valign="top"><a href="#public_key-1">public_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#sign-2">sign/2</a></td><td></td></tr><tr><td valign="top"><a href="#verify-2">verify/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="create_certificate-1"></a>

### create_certificate/1 ###

<pre><code>
create_certificate(Options) -&gt; {ok, Certificate} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Options = <a href="#type-options">options()</a></code></li><li><code>Certificate = <a href="public_key.md#type-der_encoded">public_key:der_encoded()</a></code></li><li><code>Reason = term()</code></li></ul>

<a name="der_decode-1"></a>

### der_decode/1 ###

<pre><code>
der_decode(CertificateDer) -&gt; {ok, Certificate} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>CertificateDer = <a href="public_key.md#type-der_encoded">public_key:der_encoded()</a></code></li><li><code>Certificate = term()</code></li><li><code>Reason = term()</code></li></ul>

<a name="der_encode-1"></a>

### der_encode/1 ###

<pre><code>
der_encode(Certificate) -&gt; {ok, CertificateDer} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Certificate = #OTPTBSCertificate{}</code></li><li><code>CertificateDer = <a href="public_key.md#type-der_encoded">public_key:der_encoded()</a></code></li><li><code>Reason = term()</code></li></ul>

<a name="is_self_signed-1"></a>

### is_self_signed/1 ###

<pre><code>
is_self_signed(Certificate) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Certificate = #OTPTBSCertificate{} | <a href="public_key.md#type-der_encoded">public_key:der_encoded()</a></code></li></ul>

<a name="not_after-1"></a>

### not_after/1 ###

<pre><code>
not_after(Certificate) -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>

<ul class="definitions"><li><code>Certificate = #OTPTBSCertificate{} | <a href="public_key.md#type-der_encoded">public_key:der_encoded()</a></code></li></ul>

<a name="not_before-1"></a>

### not_before/1 ###

<pre><code>
not_before(Certificate) -&gt; <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>

<ul class="definitions"><li><code>Certificate = #OTPTBSCertificate{} | <a href="public_key.md#type-der_encoded">public_key:der_encoded()</a></code></li></ul>

<a name="public_key-1"></a>

### public_key/1 ###

<pre><code>
public_key(Certificate) -&gt; {ok, <a href="onion_rsa.md#type-public_key">onion_rsa:public_key()</a>} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Certificate = #OTPTBSCertificate{} | <a href="public_key.md#type-der_encoded">public_key:der_encoded()</a></code></li><li><code>Reason = term()</code></li></ul>

<a name="sign-2"></a>

### sign/2 ###

<pre><code>
sign(Certificate, SecretKey) -&gt; CertificateDer
</code></pre>

<ul class="definitions"><li><code>Certificate = #OTPTBSCertificate{}</code></li><li><code>SecretKey = <a href="onion_rsa.md#type-secret_key">onion_rsa:secret_key()</a></code></li><li><code>CertificateDer = <a href="public_key.md#type-der_encoded">public_key:der_encoded()</a></code></li></ul>

<a name="verify-2"></a>

### verify/2 ###

<pre><code>
verify(CertificateDer, Key) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>CertificateDer = <a href="public_key.md#type-der_encoded">public_key:der_encoded()</a></code></li><li><code>Key = <a href="onion_rsa.md#type-key">onion_rsa:key()</a></code></li></ul>

