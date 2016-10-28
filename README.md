## Install

首先，自行安装erlang，版本18以上

### 获取项目代码

    git clone git@bitbucket.org:genxium/fakeoauth2server.git 

### 依赖   
通用方法，不限平台，获取和编译依赖

	cd <your_fakeoauth2server_dir>
    cd deps_source
	../rebar3 compile
	./rel.sh
	
ubuntu平台可使用预编译好的依赖，效果等价上面获取和编译依赖，但是省去clone依赖的等待时间

    cd <your_fakeoauth2server_dir>
    tar zxvf package/deps_ub14.04.tar.gz ./
    

### 编译
	
	cd <your_fakeoauth2server_dir>
	./rebar3 compile
    

## Run

    ./sh/app/fakeoauth2server live # 交互式
    ./sh/app/fakeoauth2server start # daemon模式启动
    ./sh/app/fakeoauth2server stop # 关闭
    


## 其他

- 数据文件 data/data.json
- 端口在config/app.config修改



	
	
   
