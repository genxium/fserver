## 安装软件

- erlang，版本18以上


## Init And Run

    git clone git@bitbucket.org:genxium/fakeoauth2server.git
    cd fakeoauth2server
    tar zxvf package/deps_ub14.04.tar.gz
    ./rebar3 compile
    ./sh/app/fakeoauth2server --nodename fakeoauth2server@127.0.0.1 --cmd=live


## 其他

- 数据文件 data/data.json
- 端口在config/app.config修改

