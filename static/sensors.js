class Sensors{
    constructor(){
        var self = this;
        self.cache = {};
        self.loading = {};
        if(console.log === undefined)
            self.log = ()=>{};
        else
            self.log = function(){
                var args = Array.prototype.slice.call(arguments);
                args.unshift("[Sensors]");
                return console.log.apply(console, args);
            };

        self.log("Init");

        self.apiRoot = document.querySelector("head link[rel=api-root]").getAttribute("href");
        if(!self.apiRoot){
            self.log("Failed to retrieve API root. WTF?");
        }

        var url = [location.protocol, '//', location.host, location.pathname].join('');

        self.registerElements();
    }

    registerElements(element){
        element = element || document;
        var self = this;
        self.registerAll(element, ".button.confirm", self.registerConfirm);
        self.registerAll(element, ".chart", self.registerChart);
        self.registerAll(element, ".dynamic-list", self.registerDynamicList);
    }

    loadCSS(source){
        var self = this;
        if(!self.loading[source])
            self.loading[source] = new Promise((ok)=>{
                var links = document.querySelectorAll("link[rel=stylesheet]");
                for(var i=0; i<links.length; i++){
                    if(links[i].getAttribute("href") == source){
                        ok();
                        return;
                    }
                }
                self.log("Loading", source);
                var el = self.constructElement("link",{
                    attributes: {
                        type: "text/css",
                        rel: "stylesheet",
                        crossorigin: "anonymous",
                        href: source
                    }
                });
                el.addEventListener("load", ok);
                document.querySelector("header").appendChild(el);
        });
        return self.loading[source];
    }

    loadJS(source){
        var self = this;
        if(!self.loading[source])
            self.loading[source] = new Promise((ok)=>{
                var scripts = document.querySelectorAll("script");
                for(var i=0; i<scripts.length; i++){
                    if(scripts[i].getAttribute("src") == source){
                        ok();
                        return;
                    }
                }
                self.log("Loading", source);
                var el = self.constructElement("script",{
                    attributes: {
                        type: "module",
                        crossorigin: "anonymous",
                        src: source
                    }
                });
                el.addEventListener("load", ok);
                document.querySelector("body").appendChild(el);
            });
        return self.loading[source];
    }

    apiCall(endpoint, args, methodArgs){
        var self = this;
        methodArgs = methodArgs || {};
        methodArgs.format = methodArgs.format || "json";
        return new Promise((ok, fail)=>{
            var request = new XMLHttpRequest();
            var formData;

            if(!(endpoint.startsWith("http://") || endpoint.startsWith("https://"))){
                endpoint = self.apiRoot+endpoint;
            }

            if(args instanceof HTMLElement){
                formData = new FormData(args);
                formData.delete("browser");
            }else if(args instanceof FormData){
                formData = args;
            }else{
                formData = new FormData();
                for(var field in args){
                    formData.append(field, args[field]);
                }
            }

            if(methodArgs.format == "json")
                formData.append("data-format", "json");
            request.onload = ()=>{
                var data = request.responseText;
                var status = request.status;
                if(request.getResponseHeader("Content-Type").includes("application/json")){
                    data = JSON.parse(data);
                    status = data.status || status;
                }
                if(status === 200){
                    self.log("Request succeeded", data);
                    ok(data);
                }else{
                    self.log("Request failed", data);
                    fail(data);
                }
            };
            self.log("Sending request to",endpoint,[...formData]);
            request.open("POST", endpoint);
            request.send(formData);
        });
    }

    constructElement(tag, options){
        var self = this;
        var el = document.createElement(options.tag || tag);
        (options.classes||[]).forEach(function(clas){
            if(clas) el.classList.add(clas);
        });
        if(options.text) el.innerText = options.text;
        if(options.html) el.innerHTML = options.html;
        for(var attr in (options.attributes||{})){
            if(options.attributes[attr])
                el.setAttribute(attr, options.attributes[attr]);
        }
        (options.elements||[]).forEach(function(element){
            el.appendChild(self.constructElement(element.tag, element));
        });
        return el;
    }

    populateSelect(select, data, selectedValue){
        var self = this;
        select.innerHTML = "";
        for(var i=0; i<data.length; ++i){
            select.appendChild(self.constructElement("option",{
                attributes: {value: data[i]._id, selected: (data[i]._id == selectedValue)},
                text: data[i].title || data[i].url || data[i].name + " " + data[i].address
            }));
        }
    }

    registerAll(element, query, regger){
        var self = this;
        var elements = element.querySelectorAll(query);
        for(var i=0; i<elements.length; ++i)
            regger.apply(self, [elements[i]]);
    }

    registerConfirm(element){
        element.addEventListener("click", (ev)=>{
            if(confirm("Are you sure?")){
                return true;
            }else{
                ev.preventDefault();
                return false;
            }
        });
    }

    instantiateTemplate(element){
        var copy = element.querySelector(".template").cloneNode(true);
        copy.classList.remove("template");
        copy.removeAttribute("data-name");
        [].forEach.call(copy.querySelectorAll("[data-name]"), (el)=>{
            el.setAttribute("name", el.dataset.name);
        });
        return copy;
    }

    registerDynamicList(element){
        var self = this;
        var make = ()=>{
            let copy = self.instantiateTemplate(element);
            element.querySelector("ul").appendChild(reg(copy));
            return copy;
        };
        var reg = (subelement)=>{
            self.registerElements(subelement);
            var remove = subelement.querySelector(".remove-self");
            if(remove)
                remove.addEventListener("click",()=>{
                    element.parentNode.removeChild(element);
                });
            return subelement;
        };
        [].forEach.call(element.querySelectorAll("li"), (el)=>{
            if(!el.classList.contains("template")) reg(el);
        });
        element.querySelector("a.new").addEventListener("click",make);
    }

    universalTime(date){
        let unix = Math.floor((date || Date.now()) / 1000);
        return unix+2208988800;
    }

    registerChart(element){
        let formatTimestamp = (universal)=>{
            let date = new Date((universal-2208988800)*1000);
            return date.getFullYear()+"."+(date.getMonth()+1)+"."+date.getDate()
                +" "+date.getHours()+":"+date.getMinutes();
        };
        var self = this;
        var ctx = element.querySelector("canvas").getContext("2d");
        var chart = null;
        var refresh = ()=>{
            var data = new FormData();
            [].forEach.call(element.querySelectorAll("input"), (el)=>{
                switch(el.getAttribute("type")){
                case "datetime-local":
                    if(el.value)
                        data.append(el.getAttribute("name"), self.universalTime(new Date(el.value)));
                    break;
                case "checkbox":
                    if(el.checked)
                        data.append(el.getAttribute("name"), el.value);
                    break;
                default:
                    data.append(el.getAttribute("name"), el.value);
                }
            });
            if(!data.has("time-start"))
                data.append("time-start", (self.universalTime()-(60*60*24*7))+"");
            if(!data.has("time-stop"))
                data.append("time-stop", self.universalTime()+"");
            return self.apiCall(element.getAttribute("action"), data).then((r)=>{
                let idx = {};
                chart.data.datasets = [];
                
                for(const t of r.data.measurements){
                    let type = t.device+"/"+t.type;
                    let id = idx[type];
                    if(id === undefined){
                        let axis = chart.options.scales["y"+t.type];
                        id = chart.config.data.datasets.length;
                        idx[type] = id;
                        let color = [Math.max(0,axis.color[0]-(id-2)*10),
                                     Math.max(0,axis.color[1]-(id-2)*10),
                                     Math.max(0,axis.color[2]-(id-2)*10)];
                        chart.data.datasets.push({
                            data: [],
                            backgroundColor: "rgb("+color[0]+","+color[1]+","+color[2]+")",
                            label: t.device+"/"+axis.scaleLabel.labelString,
                            yAxisID: "y"+t.type
                        });
                    }
                    chart.data.datasets[id].data.push({x: t.time, y: t.value});
                }
                chart.update();
            });
        };
        self.loadJS("https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.2.1/chart.umd.min.js")
            .then(()=>self.apiCall("type/get"))
            .then((r)=>{
                let scales = {};
                let colors = [[203, 67, 53],
                              [125, 60, 152],
                              [46, 134, 193],
                              [19, 141, 117],
                              [212, 172, 13],
                              [186, 74, 0],
                              [46, 64, 83]];
                let i = 0;
                for(const t of r.data){
                    scales["y"+t._id] = {
                        type: "linear",
                        position: "left",
                        suggestedMin: t.min,
                        suggestedMax: t.max,
                        id: "y"+t._id,
                        color: colors[i],
                        scaleLabel: {
                            display: true,
                            labelString: t.name
                        }
                    };
                    i = (i+1)%colors.length;
                }
                scales["x"] = {
                    ticks: {
                        userCallback: function(label, index, labels) {
                            return formatTimestamp(label);
                        }
                    }
                };
                chart = new Chart(ctx, {
                    type: "scatter",
                    data: {
                        datasets: []
                    },
                    options: {
                        animation: false,
                        layout: {padding: 5},
                        legend: {display: true},
                        elements: {
                            point: {
                                radius: 1
                            }
                        },
                        scales: scales,
                        tooltips: {
                            callbacks: {
                                label: function(context, data) {
                                    return data.datasets[context.datasetIndex].label+" "+formatTimestamp(context.xLabel)
                                        +": "+context.yLabel.toFixed(2)+"";
                                }
                            }
                        }
                    }
                });
                refresh();
                setInterval(refresh, 60000);
            });
        
        [].forEach.call(element.querySelectorAll("input"), (el)=>el.addEventListener("change", refresh));
    }
}

var sensors;
document.addEventListener("DOMContentLoaded", ()=>sensors = sensors || new Sensors());
