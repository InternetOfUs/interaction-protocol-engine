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

package eu.internetofus.wenet_interaction_protocol_engine.api.norms;

import eu.internetofus.common.components.profile_manager.WeNetProfileManager;
import eu.internetofus.common.components.models.WeNetUserProfile;
import eu.internetofus.common.vertx.ModelContext;
import eu.internetofus.common.vertx.ModelResources;
import eu.internetofus.common.vertx.ServiceContext;
import eu.internetofus.common.vertx.ServiceRequests;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.NormsRepository;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;

/**
 * Implements the services defined in the {@link Norms}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class NormsResource implements Norms {

  /**
   * The event bus that is using.
   */
  protected Vertx vertx;

  /**
   * The repository to manage the norms.
   */
  protected NormsRepository repository;

  /**
   * The manager to manage the users profile.
   */
  protected WeNetProfileManager profileManager;

  /**
   * Create a new instance to provide the services of the {@link Norms}.
   *
   * @param vertx where resource is defined.
   */
  public NormsResource(final Vertx vertx) {

    this.vertx = vertx;
    this.repository = NormsRepository.createProxy(vertx);
    this.profileManager = WeNetProfileManager.createProxy(vertx);
  }

  /**
   * Create the profile context.
   *
   * @return the context of the {@link WeNetUserProfile}.
   */
  protected ModelContext<PublishedNorm, String> createPublishedNormContext() {

    final var context = new ModelContext<PublishedNorm, String>();
    context.name = "publishedNorm";
    context.type = PublishedNorm.class;
    return context;

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void publishNorm(final JsonObject body, final ServiceRequest request,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var model = this.createPublishedNormContext();
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.createModel(this.vertx, body, model,
        (norm, handler) -> this.repository.storePublishedNorm(norm).onComplete(handler), context);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrievePublishedNormsPage(final String name, final String description, final String keywordsValue,
      final String publisherId, final Long publishFrom, final Long publishTo, final String orderValue, final int offset,
      final int limit, final ServiceRequest request, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    var keywords = ServiceRequests.extractQueryArray(keywordsValue);
    var order = ServiceRequests.extractQueryArray(orderValue);
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.retrieveModelsPage(offset, limit, (page, promise) -> {

      page.query = NormsRepository.createPublishedNormsPageQuery(name, description, keywords, publisherId, publishFrom,
          publishTo);
      page.sort = NormsRepository.createPublishedNormsPageSort(order);
      this.repository.retrievePublishedNormsPage(page).compose(found -> Future.succeededFuture(found.toJsonObject()))
          .onComplete(promise);

    }, context);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrievePublishedNorm(final String publishedNormId, final ServiceRequest request,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var model = this.createPublishedNormContext();
    model.id = publishedNormId;
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.retrieveModel(model, (id, handler) -> this.repository.searchPublishedNorm(id).onComplete(handler),
        context);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updatePublishedNorm(final String publishedNormId, final JsonObject body, final ServiceRequest request,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var model = this.createPublishedNormContext();
    model.id = publishedNormId;
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.updateModel(this.vertx, body, model,
        (id, handler) -> this.repository.searchPublishedNorm(id).onComplete(handler),
        (norm, handler) -> this.repository.updatePublishedNorm(norm).onComplete(handler), context);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void deletePublishedNorm(final String publishedNormId, final ServiceRequest request,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var model = this.createPublishedNormContext();
    model.id = publishedNormId;
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.deleteModel(model, (id, handler) -> this.repository.deletePublishedNorm(id).onComplete(handler),
        context);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void mergePublishedNorm(final String publishedNormId, final JsonObject body, final ServiceRequest request,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var model = this.createPublishedNormContext();
    model.id = publishedNormId;
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.mergeModel(this.vertx, body, model,
        (id, handler) -> this.repository.searchPublishedNorm(id).onComplete(handler),
        (norm, handler) -> this.repository.updatePublishedNorm(norm).onComplete(handler), context);

  }

}
