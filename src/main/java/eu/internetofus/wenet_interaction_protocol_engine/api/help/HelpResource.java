/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api.help;

import eu.internetofus.common.vertx.ServiceResponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.api.APIVerticle;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;
import java.nio.charset.Charset;
import javax.ws.rs.core.Response.Status;
import org.apache.commons.io.IOUtils;

/**
 * Resource to provide the help about the API.
 *
 * @see Help
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class HelpResource implements Help {

  /**
   * The information about the API.
   */
  protected final APIInfo info;

  /**
   * Name of the resource that contains the Open API description.
   */
  public static final String OPENA_API_RESOURCE = "wenet-interaction_protocol_engine-openapi.yaml";

  /**
   * Create a new version resource.
   *
   * @param apiVerticle where the service will be executed.
   */
  public HelpResource(final APIVerticle apiVerticle) {

    this.info = new APIInfo();
    final var conf = apiVerticle.config().getJsonObject("help", new JsonObject()).getJsonObject("info",
        new JsonObject());
    this.info.name = conf.getString("name", "wenet/interaction-protocol-engine");
    this.info.apiVersion = conf.getString("apiVersion", "Undefined");
    this.info.softwareVersion = conf.getString("softwareVersion", "Undefined");
    this.info.vendor = conf.getString("vendor", "UDT-IA, IIIA-CSIC");
    this.info.license = conf.getString("license", "Apache v2");

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void getInfo(final ServiceRequest context, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    ServiceResponseHandlers.responseOk(resultHandler, this.info);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void getOpenApi(final ServiceRequest context, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    Vertx.vertx().executeBlocking((Handler<Promise<String>>) promise -> {

      try {

        final var in = this.getClass().getClassLoader().getResourceAsStream(OPENA_API_RESOURCE);
        final var openapi = IOUtils.toString(in, Charset.defaultCharset());
        promise.complete(openapi);

      } catch (final Throwable cause) {

        promise.fail(cause);
      }

    }, res -> {

      if (res.failed()) {

        ServiceResponseHandlers.responseWithErrorMessage(resultHandler, Status.INTERNAL_SERVER_ERROR, "no_read_openapi",
            "Cannot read the OpenAPI description.");

      } else {

        final String openapi = res.result();
        resultHandler.handle(Future.succeededFuture(new ServiceResponse().setStatusCode(Status.OK.getStatusCode())
            .putHeader(HttpHeaders.CONTENT_TYPE.toString(), "application/yaml").setPayload(Buffer.buffer(openapi))));

      }
    });

  }

}
