运行环境
--

请使用Ubuntu 14.04 LTS 或 16.04.

安装Erlang-OTP 19
--

请使用[此git repo的安装脚本](https://github.com/genxium/Ubuntu14InitScripts/blob/master/backend/ejabberd-source_with_erl-otp-19/install_erlang) (含卸载指引).

Where is `<proj-root>`?
--
You're reading `<proj-root>/README.md`.

依赖   
--

- 下载及构建/编译依赖

```
# Build the dependencies from source to <proj-root>/deps_source/_build
shell@proj-root/deps_source> ../rebar3 compile

# Copy the built dependencies to <proj-root>/deps
shell@proj-root/deps_source> ./cp_libs_to_deps.sh
```

- 又或，使用Ubuntu 14.04 LTS预编译依赖 (也适用于Ubuntu 16.04)

```
shell@proj-root> tar zxvf package/deps_ub14.04.tar.gz -C .
```

编译获取服务器可执行文件
--

```
shell@proj-root> ./rebar3 compile
```

注意: 每次修改`.erl` source file(s)后均需

```
shell@proj-root> rm -rf ./_build
```


且重新编译以使更新起效.

运行
--

- Foreground Interactive

```
shell@proj-root> ./sh/app/fakeoauth2server live
```

- Background Daemon  

```
shell@proj-root> ./sh/app/fakeoauth2server start
shell@proj-root> ./sh/app/fakeoauth2server stop
```

其他
--

- 假账号数据配置文件为`data/data.conf.
- 端口配置文件为`config/app.config`修改.
- 首次使用时请执行`proj-root/overwrite_configs`以从相应的配置文件模板产生这些配置文件。
