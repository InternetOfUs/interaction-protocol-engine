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
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
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

package eu.internetofus.wenet_interaction_protocol_engine.api.versions;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response.Status;

import eu.internetofus.wenet_interaction_protocol_engine.api.APIVerticle;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.OperationRequest;
import io.vertx.ext.web.api.OperationResponse;

/**
 * Resource to provide the {@link Versions} of the API.
 *
 * @see Versions
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class VersionsResource implements Versions {

	/**
	 * The version of the software.
	 */
	protected final Version version;

	/**
	 * Create a new version resource.
	 *
	 * @param apiVerticle where the service will be executed.
	 */
	public VersionsResource(APIVerticle apiVerticle) {

		this.version = new Version();
		final JsonObject conf = apiVerticle.config().getJsonObject("version", new JsonObject());
		this.version.api = conf.getString("api", "Undefined");
		this.version.software = conf.getString("software", "Undefined");
		this.version.vendor = conf.getString("vendor", "UDT-IA,IIIA-CSIC");

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void getVersion(OperationRequest context, Handler<AsyncResult<OperationResponse>> resultHandler) {

		resultHandler.handle(Future.succeededFuture(new OperationResponse().setStatusCode(Status.OK.getStatusCode())
				.putHeader(HttpHeaders.CONTENT_TYPE.toString(), MediaType.APPLICATION_JSON)
				.setPayload(Buffer.buffer(this.version.toJsonString()))));

	}

}
