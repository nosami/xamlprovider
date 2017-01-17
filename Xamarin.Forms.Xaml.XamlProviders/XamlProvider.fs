namespace Xamarin.Forms.Xaml.XamlProviders

open System
open System.IO
open System.Reflection
open System.Collections.Generic

open Microsoft.FSharp.Core.CompilerServices

open ProviderImplementation.ProvidedTypes

open Xamarin.Forms.Build.Tasks
open Xamarin.Forms.Xaml
open Xamarin.Forms
open Xamarin.Forms.Core.UnitTests

[<TypeProvider>]
type XamlTypeProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "Xamarin.Forms.XamlProviders"
    let asm = Assembly.GetExecutingAssembly()
    let provider = ProvidedTypeDefinition(asm, ns, "XamlProvider", Some typeof<obj>, IsErased = false)
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
    
    let createFields (namesAndType: IDictionary<string,CodeDom.CodeTypeReference>) content =
        namesAndType |> List.ofSeq |> List.map (fun nameAndType ->
            // base.FindByName<Label>("myLabel") where base is ContentPage or ContentView
            let findByName = typeof<NameScopeExtensions>.GetMethod("FindByName")
            //TODO... determine the correct assembly ... code is in XamlParser.cs line 356
            let argType = Type.GetType(nameAndType.Value.BaseType + ", Xamarin.Forms.Core, Version=2.0.0.0, Culture=neutral, PublicKeyToken=null")
            let generic = findByName.MakeGenericMethod(argType)
            //content.FindByName()
            let res = generic.Invoke(content, [| content; nameAndType.Key |]);
            ProvidedProperty(propertyName = nameAndType.Key, 
                                                propertyType = argType, 
                                                IsStatic=false,
                                                GetterCode= (fun args -> <@@ res @@>)))
            //ProvidedField (nameAndType.Key, typeof<string>))
        //let label = base.FindByName<Label>("myLabel")

    let createCtor (baseType:Type)=
        //let providedConstructor = ProvidedConstructor([ProvidedParameter("handle", typeof<IntPtr>)])
        let providedConstructor = ProvidedConstructor []
        let ctorInfo = baseType.GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null, [||], null)
        providedConstructor.BaseConstructorCall <- fun args -> ctorInfo, args
        providedConstructor.InvokeCode <- fun args -> <@@ () @@>
        //providedConstructor.BaseConstructorCall <- fun args -> (fatherCtor, args)
        providedConstructor

    let createType typeName (parameterValues: obj [])=
        let xClass = parameterValues.[0] :?> string
        let allXamlFiles = Directory.EnumerateFiles (config.ResolutionFolder, "*.xaml", SearchOption.AllDirectories) |> List.ofSeq
        let xamlPick = allXamlFiles |> List.tryFind (fun path -> isMatchingXClass path xClass) 

        let xamlFile =
            match xamlPick with
            | Some file -> file
            | _ -> failwithf "Could not find a xaml file with x:Class=\"%s\"" xClass

        use stream = File.OpenText xamlFile
        let rootType, nsuri, baseCodeReference, namesAndType = XamlGTask.ParseXaml stream
        let baseType = getType baseCodeReference.BaseType "http://xamarin.com/schemas/2014/forms" asm
        let customType = ProvidedTypeDefinition (asm, ns, typeName, Some baseType, IsErased = false)
        //let customType = ProvidedTypeDefinition (asm, nsuri, xClass, Some baseType, IsErased = false)

        customType.SetAttributes (TypeAttributes.Public ||| TypeAttributes.Class)
        let content = (Activator.CreateInstance baseType :?> IControlTemplated)
        let xaml = File.ReadAllText xamlFile
        //content.LoadFromXaml(xaml) |> ignore
        //Xamarin.Forms.Init()
        //TODO: I think the line below is only required at compile time - at runtime, 
        // the user calls Xamarin.Forms.Init() from platform specific code. 
        // Also need to create our own PlatformServices. XF blows up on the Xaml load otherwise
        Xamarin.Forms.Device.PlatformServices <- new Xamarin.Forms.Core.UnitTests.MockPlatformServices()
        //Xamarin.Forms.Forms.Init(this, new Bundle())
        //content.
        XamlLoader.Load(content, xaml)
        //content.FindByName<
        createFields namesAndType content |> List.iter (fun field -> customType.AddMember field)

        customType.AddMember (createCtor baseType)
        //let tempAssembly = ProvidedAssembly( Path.ChangeExtension(Path.GetTempFileName(), ".dll"))
        //tempAssembly.AddTypes [ customType ]   
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