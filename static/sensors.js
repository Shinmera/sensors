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
                        type: "text/javascript",
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
            self.log("Sending request to",endpoint);
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

    registerChart(element){
        let formatTimestamp = (universal)=>{
            let date = new Date((universal-2208988800)*1000);
            return date.getFullYear()+"."+(date.getMonth()+1)+"."+date.getDate()
                +" "+date.getHours()+":"+date.getMinutes();
        };
        let universalTime = ()=>{
            let unix = Math.floor(Date.now() / 1000);
            return unix+2208988800;
        };
        var self = this;
        var ctx = element.querySelector("canvas").getContext("2d");
        var chart = null;
        var refresh = ()=>
            self.apiCall(element.getAttribute("action"), {
                "time-start": (universalTime()-(60*60*24*7))+""
            }).then((r)=>{
                let idx = {};
                let findLabel = (id)=>{
                    for(const axis of chart.options.scales.yAxes){
                        if(axis.id === id) return axis.scaleLabel.labelString;
                    }
                    return id;
                };
                chart.data.datasets = [];
                
                for(const t of r.data.measurements){
                    let type = t.device+"/"+t.type;
                    let id = idx[type];
                    if(id === undefined){
                        id = chart.config.data.datasets.length;
                        idx[type] = id;
                        chart.data.datasets.push({
                            data: [],
                            label: t.device+"/"+findLabel("y"+t.type),
                            yAxisID: "y"+t.type
                        });
                    }
                    chart.data.datasets[id].data.push({x: t.time, y: t.value});
                }
                chart.update();
            });
        self.loadJS("https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.3/Chart.min.js")
            .then(()=>self.loadJS("https://cdn.jsdelivr.net/npm/chartjs-plugin-colorschemes"))
            .then(()=>self.apiCall("type/get"))
            .then((r)=>{
                let axes = [];
                for(const t of r.data){
                    axes.push({
                        type: "linear",
                        position: "left",
                        min: t.min,
                        max: t.max,
                        id: "y"+t._id,
                        scaleLabel: {
                            display: true,
                            labelString: t.name
                        }
                    });
                }
                chart = new Chart(ctx, {
                    type: "scatter",
                    data: {
                        datasets: []
                    },
                    options: {
                        animation: false,
                        layout: {padding: 5},
                        legend: {display: true},
                        scales: {
                            yAxes: axes,
                            xAxes: [{
                                ticks: {
                                    userCallback: function(label, index, labels) {
                                        return formatTimestamp(label);
                                    }
                                }
                            }]
                        },
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
                setInterval(refresh, 10000);
            });
        [].forEach.call(element.querySelectorAll("select"), (el)=>{
            el.addEventListener("change", refresh);
        });
    }
}

var sensors;
document.addEventListener("DOMContentLoaded", ()=>sensors = sensors || new Sensors());
