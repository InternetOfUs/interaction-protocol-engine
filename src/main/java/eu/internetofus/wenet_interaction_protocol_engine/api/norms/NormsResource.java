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

package eu.internetofus.wenet_interaction_protocol_engine.api.norms;

import javax.ws.rs.core.Response.Status;

import eu.internetofus.wenet_interaction_protocol_engine.api.OperationReponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.NormsRepository;
import eu.internetofus.wenet_interaction_protocol_engine.services.WeNetProfileManagerService;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.OperationRequest;
import io.vertx.ext.web.api.OperationResponse;

/**
 * Implements the services defined in the {@link Norms}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class NormsResource implements Norms {

	/**
	 * The repository to manage the norms.
	 */
	protected NormsRepository repository;

	/**
	 * The manager to manage the users profile.
	 */
	protected WeNetProfileManagerService profileManager;

	/**
	 * Create an empty resource. This is only used for unit tests.
	 */
	protected NormsResource() {

	}

	/**
	 * Create a new instance to provide the services of the {@link Norms}.
	 *
	 * @param vertx where resource is defined.
	 */
	public NormsResource(Vertx vertx) {

		this.repository = NormsRepository.createProxy(vertx);
		this.profileManager = WeNetProfileManagerService.createProxy(vertx);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void publishNorm(JsonObject body, OperationRequest context,
			Handler<AsyncResult<OperationResponse>> resultHandler) {

		OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.NOT_IMPLEMENTED, "to_do",
				"Not implemented yet");

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void retrievePublishedNormsPage(OperationRequest context, Handler<AsyncResult<OperationResponse>> resultHandler) {

		OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.NOT_IMPLEMENTED, "to_do",
				"Not implemented yet");

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void retrievePublishedNorm(String publishedNormId, OperationRequest context,
			Handler<AsyncResult<OperationResponse>> resultHandler) {

		OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.NOT_IMPLEMENTED, "to_do",
				"Not implemented yet");

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void updatePublishedNorm(String publishedNormId, JsonObject body, OperationRequest context,
			Handler<AsyncResult<OperationResponse>> resultHandler) {

		OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.NOT_IMPLEMENTED, "to_do",
				"Not implemented yet");

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void deletePublishedNorm(String publishedNormId, OperationRequest context,
			Handler<AsyncResult<OperationResponse>> resultHandler) {

		OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.NOT_IMPLEMENTED, "to_do",
				"Not implemented yet");

	}

}
