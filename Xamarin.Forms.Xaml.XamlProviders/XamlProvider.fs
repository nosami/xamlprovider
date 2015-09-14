namespace Xamarin.Forms.Xaml.XamlProviders

open System
open System.IO
open System.Reflection
open System.Collections.Generic

open Microsoft.FSharp.Core.CompilerServices

open ProviderImplementation.ProvidedTypes

open Xamarin.Forms.Build.Tasks
open Xamarin.Forms.Xaml

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
    
    let getType name ns assembly =
        match Xamarin.Forms.Xaml.XamlParser.GetElementType (XmlType (ns, name, null), null, asm) with
        |t, null -> t
        |_, ex -> null
    
    let createFields (namesAndType: IDictionary<string,CodeDom.CodeTypeReference>) =
        namesAndType |> List.ofSeq |> List.map (fun nameAndType ->
            ProvidedField (nameAndType.Key, typeof<string>))

    let createCtor ()=
        let providedConstructor = ProvidedConstructor([ProvidedParameter("handle", typeof<IntPtr>)])
        let ctorInfo = typeof<obj>.GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null, [||], null)
        providedConstructor.BaseConstructorCall <- fun args -> ctorInfo, args
        providedConstructor.InvokeCode <- fun args -> <@@ () @@>
        providedConstructor

    let createType typeName (parameterValues: obj [])=
        let xClass = parameterValues.[0] :?> string
        let allXamlFiles = Directory.EnumerateFiles (config.ResolutionFolder, "*.xaml", SearchOption.AllDirectories) |> List.ofSeq
        let xamlFile = allXamlFiles |>  List.filter (fun path -> isMatchingXClass path xClass) |> List.head
        use stream = File.OpenText xamlFile
        let rootType, nsuri, baseCodeReference, namesAndType = XamlGTask.ParseXaml stream
        let baseType = getType baseCodeReference.BaseType "http://xamarin.com/schemas/2014/forms" asm
        let customType = ProvidedTypeDefinition (asm, ns, typeName, Some baseType, IsErased = false)

        namesAndType |> createFields |> List.iter (fun field -> customType.AddMember field)
        customType.AddMember (createCtor ())

        customType

    do
        provider.DefineStaticParameters (staticParameters, createType)
        this.AddNamespace (ns, [provider])

[<assembly:TypeProviderAssembly>]
do ()