

# Module onion_cell #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Onion Router Cell Utilities.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="types"></a>

## Data Types ##




### <a name="type-cell">cell()</a> ###


<pre><code>
cell() = #{circuit =&gt; <a href="#type-circuit">circuit()</a>, command =&gt; <a href="#type-command">command()</a>, payload =&gt; term()}
</code></pre>




### <a name="type-circuit">circuit()</a> ###


<pre><code>
circuit() = 0..4294967295
</code></pre>




### <a name="type-command">command()</a> ###


<pre><code>
command() = padding | create | created | relay | destroy | create_fast | created_fast | versions | netinfo | relay_early | create2 | created2 | vpadding | certs | auth_challenge | authenticate | authorize
</code></pre>




### <a name="type-error_code">error_code()</a> ###


<pre><code>
error_code() = none | protocol | internal | requested | hibernating | resource_limit | connect_failed | or_identity | or_connection_closed | finished | timeout | destroyed | no_such_service
</code></pre>




### <a name="type-relay_command">relay_command()</a> ###


<pre><code>
relay_command() = relay_begin | relay_data | relay_end | relay_connected | relay_sendme | relay_extend | relay_extended | relay_truncate | relay_truncated | relay_drop | relay_resolve | relay_resolved | relay_begin_dir | relay_extend2 | relay_extended2
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#auth_challenge-2">auth_challenge/2</a></td><td></td></tr><tr><td valign="top"><a href="#authenticate-2">authenticate/2</a></td><td></td></tr><tr><td valign="top"><a href="#authorize-0">authorize/0</a></td><td></td></tr><tr><td valign="top"><a href="#certs-1">certs/1</a></td><td></td></tr><tr><td valign="top"><a href="#create-2">create/2</a></td><td></td></tr><tr><td valign="top"><a href="#create2-2">create2/2</a></td><td></td></tr><tr><td valign="top"><a href="#create_fast-1">create_fast/1</a></td><td></td></tr><tr><td valign="top"><a href="#created-1">created/1</a></td><td></td></tr><tr><td valign="top"><a href="#created2-1">created2/1</a></td><td></td></tr><tr><td valign="top"><a href="#created_fast-2">created_fast/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td></td></tr><tr><td valign="top"><a href="#destroy-1">destroy/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-2">encode/2</a></td><td></td></tr><tr><td valign="top"><a href="#netinfo-2">netinfo/2</a></td><td></td></tr><tr><td valign="top"><a href="#netinfo-3">netinfo/3</a></td><td></td></tr><tr><td valign="top"><a href="#padding-0">padding/0</a></td><td></td></tr><tr><td valign="top"><a href="#relay-5">relay/5</a></td><td></td></tr><tr><td valign="top"><a href="#relay_early-1">relay_early/1</a></td><td></td></tr><tr><td valign="top"><a href="#versions-0">versions/0</a></td><td></td></tr><tr><td valign="top"><a href="#vpadding-0">vpadding/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="auth_challenge-2"></a>

### auth_challenge/2 ###

<pre><code>
auth_challenge(Challenge, Methods) -&gt; <a href="#type-cell">cell()</a>
</code></pre>

<ul class="definitions"><li><code>Challenge = binary()</code></li><li><code>Methods = [term()]</code></li></ul>

<a name="authenticate-2"></a>

### authenticate/2 ###

<pre><code>
authenticate(AuthType, Auth) -&gt; <a href="#type-cell">cell()</a>
</code></pre>

<ul class="definitions"><li><code>AuthType = non_neg_integer()</code></li><li><code>Auth = binary()</code></li></ul>

<a name="authorize-0"></a>

### authorize/0 ###

<pre><code>
authorize() -&gt; <a href="#type-cell">cell()</a>
</code></pre>
<br />

<a name="certs-1"></a>

### certs/1 ###

<pre><code>
certs(Certificates) -&gt; <a href="#type-cell">cell()</a>
</code></pre>

<ul class="definitions"><li><code>Certificates = [term()]</code></li></ul>

<a name="create-2"></a>

### create/2 ###

<pre><code>
create(Type, Data) -&gt; <a href="#type-cell">cell()</a>
</code></pre>

<ul class="definitions"><li><code>Type = ntor | tap</code></li><li><code>Data = binary()</code></li></ul>

<a name="create2-2"></a>

### create2/2 ###

<pre><code>
create2(Type, Data) -&gt; <a href="#type-cell">cell()</a>
</code></pre>

<ul class="definitions"><li><code>Type = ntor | tap</code></li><li><code>Data = binary()</code></li></ul>

<a name="create_fast-1"></a>

### create_fast/1 ###

<pre><code>
create_fast(KeyMaterial) -&gt; <a href="#type-cell">cell()</a>
</code></pre>

<ul class="definitions"><li><code>KeyMaterial = binary()</code></li></ul>

<a name="created-1"></a>

### created/1 ###

<pre><code>
created(Data) -&gt; <a href="#type-cell">cell()</a>
</code></pre>

<ul class="definitions"><li><code>Data = binary()</code></li></ul>

<a name="created2-1"></a>

### created2/1 ###

<pre><code>
created2(Data) -&gt; <a href="#type-cell">cell()</a>
</code></pre>

<ul class="definitions"><li><code>Data = binary()</code></li></ul>

<a name="created_fast-2"></a>

### created_fast/2 ###

<pre><code>
created_fast(KeyMaterial, DerivativeKeyData) -&gt; <a href="#type-cell">cell()</a>
</code></pre>

<ul class="definitions"><li><code>KeyMaterial = binary()</code></li><li><code>DerivativeKeyData = binary()</code></li></ul>

<a name="decode-2"></a>

### decode/2 ###

<pre><code>
decode(Version, Data) -&gt; {ok, Cell, Continuation} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Version = <a href="onion_protocol.md#type-version">onion_protocol:version()</a></code></li><li><code>Data = binary()</code></li><li><code>Cell = <a href="#type-cell">cell()</a></code></li><li><code>Continuation = binary()</code></li><li><code>Reason = term()</code></li></ul>

<a name="destroy-1"></a>

### destroy/1 ###

<pre><code>
destroy(Reason) -&gt; <a href="#type-cell">cell()</a>
</code></pre>

<ul class="definitions"><li><code>Reason = <a href="#type-error_code">error_code()</a></code></li></ul>

<a name="encode-2"></a>

### encode/2 ###

<pre><code>
encode(Version, Cell) -&gt; {ok, Data} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Version = <a href="onion_protocol.md#type-version">onion_protocol:version()</a></code></li><li><code>Cell = <a href="#type-cell">cell()</a></code></li><li><code>Data = iolist()</code></li><li><code>Reason = term()</code></li></ul>

<a name="netinfo-2"></a>

### netinfo/2 ###

<pre><code>
netinfo(TargetAddress, SourceAddresses) -&gt; <a href="#type-cell">cell()</a>
</code></pre>

<ul class="definitions"><li><code>TargetAddress = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>SourceAddresses = [<a href="inet.md#type-ip_address">inet:ip_address()</a>]</code></li></ul>

<a name="netinfo-3"></a>

### netinfo/3 ###

<pre><code>
netinfo(Timestamp, TargetAddress, SourceAddresses) -&gt; <a href="#type-cell">cell()</a>
</code></pre>

<ul class="definitions"><li><code>Timestamp = non_neg_integer()</code></li><li><code>TargetAddress = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>SourceAddresses = [<a href="inet.md#type-ip_address">inet:ip_address()</a>]</code></li></ul>

<a name="padding-0"></a>

### padding/0 ###

<pre><code>
padding() -&gt; <a href="#type-cell">cell()</a>
</code></pre>
<br />

<a name="relay-5"></a>

### relay/5 ###

<pre><code>
relay(Command, Recognized, StreamID, Digest, Data) -&gt; <a href="#type-cell">cell()</a>
</code></pre>

<ul class="definitions"><li><code>Command = <a href="#type-relay_command">relay_command()</a></code></li><li><code>Recognized = non_neg_integer()</code></li><li><code>StreamID = non_neg_integer()</code></li><li><code>Digest = non_neg_integer()</code></li><li><code>Data = binary()</code></li></ul>

<a name="relay_early-1"></a>

### relay_early/1 ###

<pre><code>
relay_early(Data) -&gt; <a href="#type-cell">cell()</a>
</code></pre>

<ul class="definitions"><li><code>Data = binary()</code></li></ul>

<a name="versions-0"></a>

### versions/0 ###

<pre><code>
versions() -&gt; <a href="#type-cell">cell()</a>
</code></pre>
<br />

<a name="vpadding-0"></a>

### vpadding/0 ###

<pre><code>
vpadding() -&gt; <a href="#type-cell">cell()</a>
</code></pre>
<br />

