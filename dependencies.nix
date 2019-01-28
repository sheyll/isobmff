{ 
	dependencies = {
		nixos = 
			import 
				(builtins.fetchTarball 
				    { 
				    	url = "https://github.com/NixOS/nixpkgs/archive/6f5bb353b6587e0624be7c84a0aa17fe51abe50e.tar.gz" ; 
           		    })
				{ };
	};
}
