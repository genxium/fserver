## 运行环境

请使用Ubuntu 14.04 LTS 或 16.04.

## 安装Erlang-OTP 19

请使用[此git repo的安装脚本](https://github.com/genxium/Ubuntu14InitScripts/blob/master/backend/ejabberd-source_with_erl-otp-19/install_erlang) (含卸载指引).

### 依赖   
- 下载及构建/编译依赖
```
    cd <your_fakeoauth2server_dir>
    cd deps_source
		../rebar3 compile
		./rel.sh
```

- 或，使用Ubuntu 14.04 LTS预编译依赖 (也适用于Ubuntu 16.04)
```
  cd <your_fakeoauth2server_dir>
  tar zxvf package/deps_ub14.04.tar.gz -C .
```

## 编译获取服务器可执行文件, 注意: 每次修改.erl source file(s)后均需重新编译以使更新起效

```
cd <your_fakeoauth2server_dir>
./rebar3 compile
```

## 运行

- Foreground Interactive
```
    ./sh/app/fakeoauth2server live
```

- Background Daemon  
```
		./sh/app/fakeoauth2server start
		./sh/app/fakeoauth2server stop
```

## 其他

- 假账号数据文件为 data/data.json.
- 使用的端口在config/app.config修改.
- 访问<host.domain.name>:8089/connect/oauth2/authorize?appid=wx5432dc1dd6164d4e&scope=snsapi_user&response_type=code&connect_redirect=1&state=dummy&redirect_uri=http%3A%2F%2Fwww.bing.com以尝试入口页面, 如 http://staging.red0769.com:8089/connect/oauth2/authorize?appid=wx5432dc1dd6164d4e&scope=snsapi_user&response_type=code&connect_redirect=1&state=dummy&redirect_uri=http%3A%2F%2Fwww.bing.com#wechat_redirect.
