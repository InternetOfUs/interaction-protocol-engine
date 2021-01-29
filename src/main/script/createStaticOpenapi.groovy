/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */
import groovy.yaml.YamlSlurper;
import groovy.yaml.YamlBuilder;


def importModelFromTo(String url,String modelName,def staticYAML,Map externalRef) {

  def modelSchema = externalRef.components.schemas[modelName];
  if( staticYAML.components.schemas[modelName] != modelSchema ){

    staticYAML.components.schemas[modelName] = modelSchema

    println "Imported $modelName from $url"

    importSubModels(url,modelSchema,staticYAML,externalRef)
  }
}

def importSubModels(String url,def modelSchema,def staticYAML,Map externalRef) {

  if( modelSchema instanceof Map){
    modelSchema.each { entry ->

      if( entry.key == "\$ref" && entry.value.startsWith("#") ){

        def modelName = entry.value.substring(entry.value.lastIndexOf('/')+1)
        importModelFromTo(url,modelName,staticYAML,externalRef)
      }else {

        importSubModels(url,entry.value,staticYAML,externalRef)
      }
    }
  }else if( modelSchema instanceof Collection){

    modelSchema.each { element ->

      importSubModels(url,element,staticYAML,externalRef)
    }
  }
}

if( generatedOpenApi == null ){

  println "You must define the generatedOpenApi property"
  System.exit(-1)
}

if( staticOpenApi == null ){

  println "You must define the staticOpenApi property"
  System.exit(-1)
}

File generatedOpenApiFile = new File(generatedOpenApi)
File staticOpenApiFile = new File(staticOpenApi)

if( staticOpenApiFile.exists() && staticOpenApiFile.lastModified() >= generatedOpenApiFile.lastModified()){
  //Nothing to do
  return;
}
staticOpenApiFile.delete()

String text = generatedOpenApiFile.text

def externalRefPattern = /[\"\']?(http.+\.yaml)#\/components\/schemas\/(\S+)[\"\']?/
def externalPatterns = (text =~ externalRefPattern).findAll() as Set

text = text.replaceAll(externalRefPattern, /"#\/components\/schemas\/$2"/)

YamlSlurper yaml = new YamlSlurper()
def staticYAML =  yaml.parseText(text)

def externalRefs = [:]
for( pattern in externalPatterns){

  String url = pattern[1]
  def externalRef = externalRefs[url]
  if( externalRef == null ){

    externalRef = yaml.parse(url.toURL().openStream())
    externalRefs[url] = externalRef
  }

  String modelName = pattern[2]
  importModelFromTo(url,modelName,staticYAML,externalRef)
}


def builder = new YamlBuilder()
builder staticYAML
staticOpenApiFile << builder
println "Generated:"+staticOpenApiFile