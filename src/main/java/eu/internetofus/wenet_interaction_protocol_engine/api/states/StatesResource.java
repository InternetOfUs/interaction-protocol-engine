/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 1994 - 2021 UDT-IA, IIIA-CSIC
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
package eu.internetofus.wenet_interaction_protocol_engine.api.states;

import eu.internetofus.common.TimeManager;
import eu.internetofus.common.components.Merges;
import eu.internetofus.common.components.interaction_protocol_engine.State;
import eu.internetofus.common.components.interaction_protocol_engine.StatesPage;
import eu.internetofus.common.vertx.ModelContext;
import eu.internetofus.common.vertx.ModelResources;
import eu.internetofus.common.vertx.ServiceContext;
import eu.internetofus.common.vertx.ServiceResponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.StatesRepository;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;

/**
 * Implementation of the {@link States}
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class StatesResource implements States {

  /**
   * The event bus that is using.
   */
  protected Vertx vertx;

  /**
   * Create a new resource.
   *
   * @param vertx event bus to use.
   */
  public StatesResource(final Vertx vertx) {

    this.vertx = vertx;

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrieveStates(final String communityId, final String taskId, final String userId,
      final ServiceRequest context, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    ServiceResponseHandlers.responseOk(resultHandler, new StatesPage());

  }

  /**
   * Retrieve an state.
   *
   * @param communityId   identifier of the community.
   * @param taskId        identifier of the task.
   * @param userId        identifier of the user.
   * @param resultHandler to notify of the result.
   */
  protected void retrieveState(final String communityId, final String taskId, final String userId,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    StatesRepository.createProxy(this.vertx).searchState(communityId, taskId, userId).onComplete(search -> {

      var state = search.result();
      if (state == null) {

        state = new State();
        state.communityId = communityId;
        state.taskId = taskId;
        state.userId = userId;
      }

      ServiceResponseHandlers.responseOk(resultHandler, state);
    });

  }

  /**
   * Merge an state.
   *
   * @param communityId   identifier of the community.
   * @param taskId        identifier of the task.
   * @param userId        identifier of the user.
   * @param body          that has received.
   * @param request       that has been received.
   * @param resultHandler to notify of the result.
   */
  protected void mergeState(final String communityId, final String taskId, final String userId, final JsonObject body,
      final ServiceRequest request, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var model = new ModelContext<State, Void>();
    model.name = "state";
    model.type = State.class;
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.toModel(body, model, context, () -> {

      StatesRepository.createProxy(this.vertx).searchState(communityId, taskId, userId).onComplete(search -> {

        final var state = search.result();
        if (state == null) {

          model.source.communityId = communityId;
          model.source.taskId = taskId;
          model.source.userId = userId;
          model.source._creationTs = model.source._lastUpdateTs = TimeManager.now();
          StatesRepository.createProxy(this.vertx).storeState(model.source).onComplete(stored -> {

            ServiceResponseHandlers.responseOk(resultHandler, stored.result());

          });

        } else {

          state.attributes = Merges.mergeJsonObjects(state.attributes, model.source.attributes);
          state._lastUpdateTs = TimeManager.now();

          StatesRepository.createProxy(this.vertx).updateState(state).onComplete(updated -> {

            ServiceResponseHandlers.responseOk(resultHandler, state);

          });
        }

      });

    });
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrieveCommunityUserState(final String communityId, final String userId, final ServiceRequest context,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    this.retrieveState(communityId, null, userId, resultHandler);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void mergeCommunityUserState(final String communityId, final String userId, final JsonObject body,
      final ServiceRequest request, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    this.mergeState(communityId, null, userId, body, request, resultHandler);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrieveTaskUserState(final String taskId, final String userId, final ServiceRequest context,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    this.retrieveState(null, taskId, userId, resultHandler);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void mergeTaskUserState(final String taskId, final String userId, final JsonObject body,
      final ServiceRequest request, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    this.mergeState(null, taskId, userId, body, request, resultHandler);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrieveUserState(final String userId, final ServiceRequest context,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    this.retrieveState(null, null, userId, resultHandler);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void mergeUserState(final String userId, final JsonObject body, final ServiceRequest request,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    this.mergeState(null, null, userId, body, request, resultHandler);

  }

}
