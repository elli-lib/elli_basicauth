

# Module elli_basicauth #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Elli basicauth middleware.

Copyright (c) 2013, Martin Rehfeld; 2018, elli-lib team

This middleware provides basic authentication to protect
requests, based on a user-configured authentication function.

__Behaviours:__ [`elli_handler`](https://github.com/elli-lib/elli/blob/develop/doc/elli_handler.md).

__Authors:__ Martin Rehfeld, Eric Bailey.

<a name="types"></a>

## Data Types ##




### <a name="type-auth_fun">auth_fun()</a> ###


__abstract datatype__: `auth_fun()`

A user-configurable authentication function.



### <a name="type-auth_status">auth_status()</a> ###


__abstract datatype__: `auth_status()`

The result of an <code><a href="#type-auth_fun">auth_fun()</a></code>.



### <a name="type-config">config()</a> ###


__abstract datatype__: `config()`

A property list of options.
The configurable options are:



<dt><code>auth_fun</code></dt>




<dd>An <code><a href="#type-auth_fun">auth_fun()</a></code></dd>




<dt><code>auth_realm</code></dt>




<dd>A binary <a href="https://tools.ietf.org.md/rfc1945#section-11">realm</a>.</dd>



<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default_auth_fun-3">default_auth_fun/3</a></td><td>Default to <code>forbidden</code>, in case of missing <code>auth_fun</code> config.</td></tr><tr><td valign="top"><a href="#handle-2">handle/2</a></td><td>Protect <code>Req</code> based on the configured <code>auth_fun</code>.</td></tr><tr><td valign="top"><a href="#handle_event-3">handle_event/3</a></td><td>No-op to satisfy the <code>elli_handler</code> behaviour.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="default_auth_fun-3"></a>

### default_auth_fun/3 ###

<pre><code>
default_auth_fun(Req, User, Password) -&gt; AuthStatus
</code></pre>

<ul class="definitions"><li><code>Req = <a href="http://raw.github.com/elli-lib/elli/develop/doc/elli.md#type-req">elli:req()</a></code></li><li><code>User = binary()</code></li><li><code>Password = binary()</code></li><li><code>AuthStatus = <a href="#type-auth_status">auth_status()</a></code></li></ul>

Default to `forbidden`, in case of missing `auth_fun` config.

<a name="handle-2"></a>

### handle/2 ###

<pre><code>
handle(Req::<a href="http://raw.github.com/elli-lib/elli/develop/doc/elli.md#type-req">elli:req()</a>, Config::<a href="#type-config">config()</a>) -&gt; <a href="http://raw.github.com/elli-lib/elli/develop/doc/elli_handler.md#type-result">elli_handler:result()</a>
</code></pre>
<br />

Protect `Req` based on the configured `auth_fun`.
If none is given, the default authentication is `forbidden`.

<a name="handle_event-3"></a>

### handle_event/3 ###

<pre><code>
handle_event(Event::<a href="http://raw.github.com/elli-lib/elli/develop/doc/elli_handler.md#type-event">elli_handler:event()</a>, Args::list(), Config::<a href="#type-config">config()</a>) -&gt; ok
</code></pre>
<br />

No-op to satisfy the `elli_handler` behaviour. Return `ok`.

