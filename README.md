## Install

首先，自行安装erlang，版本18以上

### 获取项目代码

    git clone git@bitbucket.org:genxium/fakeoauth2server.git 

### 依赖   
- 下载及编译获取
```
通用方法，不限平台，获取和编译依赖

		cd <your_fakeoauth2server_dir>
		cd deps_source
		../rebar3 compile
		./rel.sh
```

- 使用Ubuntu 14.04 LTS预编译依赖
```
    cd <your_fakeoauth2server_dir>
    tar zxvf package/deps_ub14.04.tar.gz -C .
``` 

## 编译获取服务器可执行文件, 每次修改.erl source file(s)后均需重新编译以使更新起效
	
		cd <your_fakeoauth2server_dir>
		./rebar3 compile
    

## 运行 

- Foreground Interactive 
```
    ./sh/app/fakeoauth2server live # 交互式
```

- Background Daemon  
```
		./sh/app/fakeoauth2server start
		./sh/app/fakeoauth2server stop
```
    
## 其他

- 数据文件 data/data.json
- 端口在config/app.config修改
- 访问<host.domain.name>:8089/connect/oauth2/authorize?appid=wx5432dc1dd6164d4e&scope=snsapi_user&response_type=code&connect_redirect=1&state=dummy&redirect_uri=http%3A%2F%2Fwww.bing.com以尝试入口页面, 如 http://staging.red0769.com:8089/connect/oauth2/authorize?appid=wx5432dc1dd6164d4e&scope=snsapi_user&response_type=code&connect_redirect=1&state=dummy&redirect_uri=http%3A%2F%2Fwww.bing.com。 



	
	
   
