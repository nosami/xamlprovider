namespace Xamarin.Forms.Xaml.XamlProviders

open System
open System.IO
open System.Reflection
open System.Collections.Generic

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open ProviderImplementation.ProvidedTypes

open Xamarin.Forms.Build.Tasks
open Xamarin.Forms.Xaml
open Xamarin.Forms
open Xamarin.Forms.Core.UnitTests

module XamlModule =
    let elements = Dictionary<string, Element>()
    let getElement name xClass =
        if elements.ContainsKey name 
        then
            elements.[name]
        else 
            elements.[name]

[<TypeProvider>]
type XamlTypeProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "Xamarin.Forms"
    let asm = Assembly.GetExecutingAssembly()
    let provider = ProvidedTypeDefinition(asm, ns, "XAML", Some typeof<obj>, IsErased = false)
    let staticParameters= [ProvidedStaticParameter ("xClass", typeof<string>)]

    let isMatchingXClass path xClass=
        use stream = File.OpenText path
        match XamlGTask.ParseXaml stream with
            |null, null, null, null -> false
            |rootType, ns, baseType, namesAndTypes -> rootType = xClass
    
    let getType qualifiedName ns assembly =
        let name =
            match qualifiedName with
            | "Xamarin.Forms.ContentPage" -> "ContentPage"
            | "Xamarin.Forms.ContentView" -> "ContentView"
            | _ -> failwithf "Don't know how to handle %s" qualifiedName

        match Xamarin.Forms.Xaml.XamlParser.GetElementType (XmlType (ns, name, null), null, asm) with
        | t, null -> t
        | _, ex -> null
    
    let createFields xClass (namesAndType: IDictionary<string,CodeDom.CodeTypeReference>) (content:Element) (customType:Type) =
        namesAndType |> List.ofSeq |> List.map (fun nameAndType ->
            // base.FindByName<Label>("myLabel") where base is ContentPage or ContentView
            let findByName = typeof<NameScopeExtensions>.GetMethod("FindByName")
            //let findByName = customType.BaseType.GetMethod("FindByName")
            //TODO... determine the correct assembly ... code is in XamlParser.cs line 356
            let elementType = Type.GetType(nameAndType.Value.BaseType + ", Xamarin.Forms.Core, Version=2.0.0.0, Culture=neutral, PublicKeyToken=null")
            let generic = findByName.MakeGenericMethod(elementType)
            //let generic = findByName.MakeGenericMethod(typeof<Element>)
            //content.FindByName()
            let propertyName = nameAndType.Key
            let res = generic.Invoke(null, [| content; propertyName |])
            XamlModule.elements.[propertyName] <- unbox res

            //let p = ContentPage()
            //let o = p.FindByName "label"
            //let elem = content.FindByName("myLabel")
            //Expr.Prop
            //let accessExpr (args:Expr list) =
            //    let name = nameAndType.Key
            //    let this = args.[0]
            //    let expr = Expr.Call(generic, [ Expr.Value content; Expr.Value propertyName ] )
                //let thisAsBase = Expr.Coerce(content, argType)
                //let field = Expr.FieldGet(thisAsBase, fi)
                //let arg = Expr.Value(res)
                //let expr = Expr.Call(content, generic, [arg])
                //Expr.Coerce(expr, argType)
                //expr
            //expr.
            //let propertyGet = Expr.P
            //let field = ProvidedField(fieldName = nameAndType.Key, fieldType = elementType)
            //field.SetValue(this, res)
            //field, res, elementType)
            let accessExpr (args:Expr list) =
                let name = nameAndType.Key
                let this = args.[0]
                let thisAsBase = Expr.Coerce(this, customType.BaseType)
                //Expr.
                //let field = Expr.Application() .FieldGet(thisAsBase, fi)
                let arg = Expr.Value(name)
                let expr = Expr.Call(generic, [thisAsBase; arg])
                Expr.Coerce(expr, elementType)

            //for node in elements do
            //    let property = 
            printf "%s %s" propertyName elementType.FullName
            ProvidedProperty(
                propertyName = nameAndType.Key,
                propertyType = elementType,
                GetterCode = accessExpr))

            //ProvidedProperty(propertyName = nameAndType.Key, 
            //                                    propertyType = elementType, 
            //                                    IsStatic=false,
            //                                    GetterCode = accessExpr))
                                                //GetterCode= (fun args -> <@@ res @@>)))
                                                //GetterCode= (fun args -> <@@ XamlModule.getElement propertyName xClass @@>)))
            //ProvidedField (nameAndType.Key, typeof<string>))
        //let label = base.FindByName<Label>("myLabel")

    let createCtor (baseType:Type) = // (fields: (ProvidedField * Object * Type) list)=
        //let providedConstructor = ProvidedConstructor([ProvidedParameter("handle", typeof<IntPtr>)])
        let providedConstructor = ProvidedConstructor []
        let ctorInfo = baseType.GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null, [||], null)
        providedConstructor.BaseConstructorCall <- fun args -> ctorInfo, args
        providedConstructor.InvokeCode <- 
            fun args -> 
                match args with 
                | [this] ->
                    //<@@
                    //    fields |> 
                    //    List.iter(fun (field, elem) -> field.SetValue(this, elem))
                    //@@>
                    let loadFromXml = typeof<Extensions>.GetMethod("LoadFromXaml")
                    let genericLoadFromXml = loadFromXml.MakeGenericMethod(baseType)
                    let getType = typeof<Type>.GetMethod("GetType", [| typeof<string> |])
                    let getTypeExpr = Expr.Call(getType, [Expr.Value("fsxaml8.XamlPage")])
                    let thisAsBase = Expr.Coerce(this, baseType)
                    //let arg = Expr.Value("fsxaml8.fsxaml8Page")
                    let expr = Expr.Call(genericLoadFromXml, [thisAsBase; getTypeExpr])
                    expr
                    //Expr.Coerce(expr, elementType)
                    //let f, e, t = fields |> List.head
                    //Expr.FieldSet(this, f, Expr.Coerce(Expr.Value e, t))
                    //<@@ () @@>
                | _ -> failwith "Wrong constructor arguments"
            //fun args -> <@@ () @@>
        //providedConstructor.BaseConstructorCall <- fun args -> (fatherCtor, args)
        providedConstructor

    let createType typeName (parameterValues: obj [])=
        let xClass = string parameterValues.[0]
        let allXamlFiles = Directory.EnumerateFiles (config.ResolutionFolder, "*.xaml", SearchOption.AllDirectories) |> List.ofSeq
        let xamlFileOption = allXamlFiles |> List.tryFind (fun path -> isMatchingXClass path xClass) 
        printf "here"
        let xamlFile =
            match xamlFileOption with
            | Some file -> file
            | _ -> 
                printf "Could not find a xaml file with x:Class=\"%s\"" xClass
                failwithf "Could not find a xaml file with x:Class=\"%s\"" xClass
        printf "here2"
        use stream = File.OpenText xamlFile
        let rootType, nsuri, baseCodeReference, namesAndType = XamlGTask.ParseXaml stream
        let baseType = getType baseCodeReference.BaseType "http://xamarin.com/schemas/2014/forms" asm
        let customType = ProvidedTypeDefinition (asm, ns, typeName, Some baseType, IsErased = false)
        //let customType = ProvidedTypeDefinition (asm, nsuri, xClass, Some baseType, IsErased = false)

        customType.SetAttributes (TypeAttributes.Public ||| TypeAttributes.Class)
        let content = (Activator.CreateInstance baseType :?> ContentPage)
        let xaml = File.ReadAllText xamlFile
        //content.LoadFromXaml(xaml) |> ignore
        //Xamarin.Forms.Init()
        //TODO: I think the line below is only required at compile time - at runtime, 
        // the user calls Xamarin.Forms.Init() from platform specific code. 
        // Also need to create our own PlatformServices. XF blows up on the Xaml load otherwise
        //if Xamarin.Forms.Device.PlatformServices = null then
        Xamarin.Forms.Device.PlatformServices <- new Xamarin.Forms.Core.UnitTests.MockPlatformServices()
        //Xamarin.Forms.Forms.Init(this, new Bundle())
        //content.LoadFromXaml()
        XamlLoader.Load(content, xaml)
        //content.FindByName<
        let fields = createFields xClass namesAndType content customType
        //fields |> List.iter (fun (field, _elem, _type) -> customType.AddMember field)
        fields |> List.iter (fun field -> customType.AddMember field)
        customType.AddMember (createCtor baseType)// fields)
        let tempAssembly = ProvidedAssembly( Path.ChangeExtension(Path.GetTempFileName(), ".dll"))
        tempAssembly.AddTypes [ provider ]   
        tempAssembly.AddTypes [ customType ]   
        customType

    do
        //let tempAsmPath = Path.ChangeExtension(Path.GetTempFileName(), ".dll")
        //let tempAsm = ProvidedAssembly "/Users/jason/temp.dll"
        //ProvidedAssembly.RegisterGenerated("/Users/jason/temp.dll")
        provider.DefineStaticParameters (staticParameters, createType)
        this.RegisterRuntimeAssemblyLocationAsProbingFolder config
        //tempAsm.AddTypes [provider]
        this.AddNamespace (ns, [provider])

[<assembly:TypeProviderAssembly>]
do ()