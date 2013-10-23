Purpose:
=========

mod_zeropush sends an API request to ZeroPush when a message is sent to an offline user.

The notification contains the following URI-encoded payload:

```json
{
	"auth_token": "{config-auth_token}",
	"channel": "{recipient-jid}",
	"alert": "{chat-message-body}",
	"badge": "+1",
	"sound": "{config-sound}",
	"info": {
		"from": "{sender-jid}"
	}
}
```

Caveats:
=========

mod_zeropush assumes that you have created broadcast channels with the user's jabber id that represent the devices you would like to notify.

Please read [register](https://zeropush.com/documentation/api_reference#register) and [subscribe](https://zeropush.com/documentation/api_reference#subscribe) how to use broadcast channels.

Note:
==========

Between ejabberd 2.1.13 and 13.10 there were significant changes and modules compiled for one version don't directly work with the other version.
Checkout the branch that corresponds to the version of ejabberd that you are running.

Installing:
==========

* Make sure you have erlang installed on the machine you are building from
  * You probably want this to be the same machine you intend to install/run ejabberd on. I'm not sure about the interoperability of ejabberd/erlang versions.
* Open the Emakefile and change `/usr/local/Cellar/ejabberd/2.1.13/lib/ejabberd/include` to the correct path on your machine
* Run the `./build.sh` to build `*.beam` files
* Copy the `*.beam` files from the `ebin` directory to the location where the other modules are for your server
* Add the configuration from below

eJabberd 13.10
===

Configuration
---

in `ejabberd.yml`

```yml
mod_zeropush:
	sound: "default"
	auth_token: "your-auth-token"
	post_url: "https://api.zeropush.com/broadcast"
```

eJabberd 2.1.13
===

Configuration
---

in `ejabberd.cfg`

```erlang
{mod_zeropush, [
	{sound, "default"},
	{auth_token, "your-auth-token"},
	{post_url, "https://api.zeropush.com/broadcast"}
]}
```

