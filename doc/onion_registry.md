

# Module onion_registry #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Process Registry API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#await-1">await/1</a></td><td>Wait until a process have registered under the given name.</td></tr><tr><td valign="top"><a href="#await-2">await/2</a></td><td>Wait until a process have registered under the given name.</td></tr><tr><td valign="top"><a href="#lookup-1">lookup/1</a></td><td>Lookup a given name and return the process identifier.</td></tr><tr><td valign="top"><a href="#register-1">register/1</a></td><td>Register the current process under the given name.</td></tr><tr><td valign="top"><a href="#unregister-1">unregister/1</a></td><td>Unregister the current process under the given name.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="await-1"></a>

### await/1 ###

<pre><code>
await(Name) -&gt; {ok, pid()} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Name = term()</code></li><li><code>Reason = term()</code></li></ul>

Wait until a process have registered under the given name.

<a name="await-2"></a>

### await/2 ###

<pre><code>
await(Name, Timeout) -&gt; {ok, pid()} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Name = term()</code></li><li><code>Timeout = non_neg_integer()</code></li><li><code>Reason = term()</code></li></ul>

Wait until a process have registered under the given name.

<a name="lookup-1"></a>

### lookup/1 ###

<pre><code>
lookup(Name) -&gt; {ok, pid()} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Name = term()</code></li><li><code>Reason = term()</code></li></ul>

Lookup a given name and return the process identifier.

<a name="register-1"></a>

### register/1 ###

<pre><code>
register(Name) -&gt; any()
</code></pre>

<ul class="definitions"><li><code>Name = term()</code></li></ul>

Register the current process under the given name.

<a name="unregister-1"></a>

### unregister/1 ###

<pre><code>
unregister(Name) -&gt; true
</code></pre>

<ul class="definitions"><li><code>Name = term()</code></li></ul>

Unregister the current process under the given name.

